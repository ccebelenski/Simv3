package com.sim.device

import java.nio.ByteBuffer
import java.nio.file.Path

import com.sim.Utils

/*
IMAGE FILE FORMAT

       The overall layout of an ImageDisk .IMD image file is:

         IMD v.vv: dd/mm/yyyy hh:mm:ss          (ASCII Header)
         Comment (ASCII only - unlimited size)  (NOTE:  You can TYPE a .IMD)
         1A byte - ASCII EOF character          (file to see header/comment)
         - For each track on the disk:
            1 byte  Mode value                  (0-5)
            1 byte  Cylinder                    (0-n)
            1 byte  Head                        (0-1)   (see Note)
            1 byte  number of sectors in track  (1-n)
            1 byte  sector size                 (0-6)
            sector numbering map                * number of sectors
            sector cylinder map (optional)      * number of sectors
            sector head map     (optional)      * number of sectors
            sector data records                 * number of sectors
         <End of file>

       6.1 Mode value

          This value indicates the data transfer rate and density  in  which
          the original track was recorded:

             00 = 500 kbps FM   \   Note:   kbps indicates transfer rate,
             01 = 300 kbps FM    >          not the data rate, which is
             02 = 250 kbps FM   /           1/2 for FM encoding.
             03 = 500 kbps MFM
             04 = 300 kbps MFM
             05 = 250 kbps MFM

       6.2 Sector size

          The Sector Size value indicates the actual size of the sector data
          occuring on the track:

            00 =  128 bytes/sector
            01 =  256 bytes/sector
            02 =  512 bytes/sector
            03 = 1024 bytes/sector
            04 = 2048 bytes/sector
            05 = 4096 bytes/sector
            06 = 8192 bytes/sector

          ImageDisk does not currently handle disks with  differently  sized
          sectors within the same  track  (the  PC  FDC  cannot  write  such
          disks),  however an extension to the .IMD  file  format  has  been
          suggested to allow these type of disks to be represented:

             A sector size value of 0xFF indicates that a  table  of  sector
             sizes occurs  after  the  sector  numbering/cylinder/head  maps
             (immediately before the data records) which contains one 16-bit
             value  (in little endian format)  per sector which defines  the
             actual size of that sector.

       6.3 Head value

          This value indicates the side of the  disk  on  which  this  track
          occurs (0 or 1).

          Since HEAD can only be 0 or 1,  ImageDisk uses the upper  bits  of
          this byte to indicate the presense of optional items in the  track
          data:

             Bit 7 (0x80) = Sector Cylinder Map
             Bit 6 (0x40) = Sector Head     Map

       6.4 Sector numbering map

          The sector numbering map contains one byte  entry  containing  the
          physical ID for each sector that occurs in the track.

          Note that these entries may NOT be sequential.  A disk which  uses
          sector interleave will have a sector numbering map  in  which  the
          sector numbers occur in non-sequential order.

          If ImageDisk is unable to obtain all sector numbers  in  a  single
          revolution of the  disk,  it  will  report  "Unable  to  determine
          interleave"  and  rearrange  the  sector  numbers  into  a  simple
          sequential list.

       6.5 Sector Cylinder Map

          This is an optional field.  It's presense is indicated  by  bit  7
          being set in the Head value for the track.

          When present,  it means that the cylinder values  written  to  the
          sectors do NOT match the physical cylinder of the track.

          The Sector Cylinder  Map  has  one  entry  for  each  sector,  and
          contains the logical Cylinder ID for the corresponding  sector  in
          the Sector Numbering Map.

          Reading a disk with non-standard Cylinder ID's  will  require  the
          use of the FULL ANALYSIS setting.

       6.6 Sector Head map

          This is an optional field.  It's presense is indicated  by  bit  6
          being set in the Head value for the track.

          When present, it means that the head values written to the sectors
          do NOT match the physical head selection of the track.

          The Sector Head Map has one entry for each  sector,  and  contains
          the logical Head ID for the corresponding  sector  in  the  Sector
          Numbering Map.

          Reading a disk with non-standard Head ID's may require the use  of
          the FULL ANALYSIS setting.

       6.7 Sector Data Records

          For each sector ID occuring in the Sector Numbering Map, ImageDisk
          records a Sector Data Record - these records  occur  in  the  same
          order as the IDs in the Sector Numbering Map:

            00      Sector data unavailable - could not be read
            01 .... Normal data: (Sector Size) bytes follow
            02 xx   Compressed: All bytes in sector have same value (xx)
            03 .... Normal data with "Deleted-Data address mark"
            04 xx   Compressed  with "Deleted-Data address mark"
            05 .... Normal data read with data error
            06 xx   Compressed  read with data error
            07 .... Deleted data read with data error
            08 xx   Compressed, Deleted read with data error

 */
trait ImageDisk {

  val IMAGE_TYPE_DSK = 1 // Flat binary "DSK" image file.
  val IMAGE_TYPE_IMD = 2 // ImageDisk "IMD" image file.
  val IMAGE_TYPE_CPT = 3 // CP/M Transfer "CPT" image file.

  var IMAGE_TYPE: Int = IMAGE_TYPE_DSK

  private val SECT_RECORD_UNAVAILABLE = 0 // Data could not be read from the original media
  private val SECT_RECORD_NORM = 1 // Normal Data
  private val SECT_RECORD_NORM_COMP = 2 // Compressed Normal Data
  private val SECT_RECORD_NORM_DAM = 3 // Normal Data with deleted address mark
  private val SECT_RECORD_NORM_DAM_COMP = 4 // Compressed Normal Data with deleted address mark
  private val SECT_RECORD_NORM_ERR = 5 // Normal Data
  private val SECT_RECORD_NORM_COMP_ERR = 6 // Compressed Normal Data
  private val SECT_RECORD_NORM_DAM_ERR = 7 // Normal Data with deleted address mark
  private val SECT_RECORD_NORM_DAM_COMP_ERR = 8 // Compressed Normal Data with deleted address mark

  private val IMD_DISK_IO_ERROR_GENERAL = 1 << 0 // General data error.
  private val IMD_DISK_IO_ERROR_CRC = 1 << 1 // Data read/written, but got a CRC error.
  private val IMD_DISK_IO_DELETED_ADDR_MARK = 1 << 2 // Sector had a deleted address mark
  private val IMD_DISK_IO_COMPRESSED = 1 << 3 // Sector is compressed in the IMD file (Read Only)
  private val IMD_DISK_IO_ERROR_WPROT = 1 << 4 // Disk is write protected

  private val IMD_FLAG_SECT_HEAD_MAP = 1 << 6
  private val IMD_FLAG_SECT_CYL_MAP = 1 << 7

  private val MAX_SPT = 26

  var diskInfo: DiskInfo = new DiskInfo()
  var diskComment: Option[String] = None

  final def isIMD():Boolean = {
    IMAGE_TYPE == IMAGE_TYPE_IMD
  }

  /* Open an existing IMD disk image.  It will be opened and parsed, and after this
 * call, will be ready for sector read/write. The result is the corresponding
 * DISK_INFO or NULL if an error occurred.
 */
  def diskOpen(unit: DiskUnit): Unit = {
    if (unit.fileChannel == null || !unit.fileChannel.isOpen) return
    diskParse(unit)
  }


  /* Scans the IMD file for the comment string, and returns it in comment buffer.
   * After this function returns, the file pointer is placed after the comment and
   * the 0x1A "EOF" marker.
   *
   * The comment parameter is optional, and if null, then the comment will not
   * be extracted from the IMD file, but the file position will still be advanced
   * to the end of the comment.
   *
   */
  def commentParse(disk: DiskUnit): Option[String] = {
    if (!disk.fileChannel.isOpen) return None
    // rewind to beginning
    disk.fileChannel.position(0)
    val sb = new StringBuilder
    // If we don't find the terminator in the first 1k then we will fail...
    // Just for simplicity, as I suppose some weirdness could happen.
    val buf = ByteBuffer.allocate(1024)
    var bytesread = 0
    while (buf.hasRemaining && bytesread > 0) {
      bytesread = disk.fileChannel.read(buf)
    }
    val a = buf.array()
    val idx = a.indexOf(0x1a)
    disk.fileChannel.position(idx) // TODO Check if this is correct
    for (i <- 0 to idx) {
      sb.append(a(i))
    }

    buf.clear()
    Some(sb.toString())
  }

  val MAX_CYL = 80
  val MAX_HEAD = 2
  val MAX_COMMENT_LEN = 256

  def headerOK(imd: IMD_HEADER): Boolean = {
    (imd.cyl < MAX_CYL) && (imd.head < MAX_HEAD)
  }

  /* Parse an IMD image.  This sets up for us to be able to do sector read/write and
   * track write.
   */
  def diskParse(diskUnit: DiskUnit): Unit = {
    //var comment: Array[Byte] = Array.ofDim(256)
    var sectorMap: Array[Byte] = Array.ofDim(256)
    var sectorHeadMap: Array[Byte] = Array.ofDim(256)
    var sectorCylMap: Array[Byte] = Array.ofDim(256)
    var sectorSize = 0
    var sectorHeadwithFlags = 0
    var sectRecordType = 0
    var start_sect = 0

    var TotalSectorCount = 0
    val imd: IMD_HEADER = new IMD_HEADER()

    diskInfo.track = Array.ofDim(MAX_CYL, MAX_HEAD)

    diskComment = commentParse(diskUnit)
    diskComment match {
      case None =>
      case Some(x: String) => Utils.outln(s"IMD: $x")
    }

    diskInfo.nsides = 1
    diskInfo.ntracks = 0
    diskInfo.flags = 0 /* Make sure all flags are clear. */

    if (diskUnit.fileChannel.size() <= 0) {
      Utils.outln("IMD: Disk image is blank, it must be formatted.\r\n")
      return
    }

    while (diskUnit.fileChannel.position != diskUnit.fileChannel.size()) {
      Utils.outlnd(diskUnit, s"IMD: start of track ${diskInfo.ntracks} at file offset ${diskUnit.fileChannel.position()}")
      val bufhead = ByteBuffer.allocate(5)
      if (diskUnit.fileChannel.read(bufhead) != 5) {
        Utils.outln(s"IMD: Could not read IMD header.")
        return
      }
      // break out the header bytes to imd structure
      val harr = bufhead.array()
      imd.mode = harr(0)
      imd.cyl = harr(1)
      imd.head = harr(2)
      imd.nsects = harr(3)
      imd.sectsize = harr(4)
      bufhead.clear()

      sectorSize = 128 << (imd.sectsize & 0x1f)
      sectorHeadwithFlags = imd.head /* save the head and flags */
      imd.head &= 1 /* mask out flag bits to head 0 or 1 */

      Utils.outlnd(diskUnit, s"Track ${diskInfo.ntracks}:")
      Utils.outlnd(diskUnit, s"\tMode=${imd.mode}, Cyl=${imd.cyl}, Head=${sectorHeadwithFlags}(${imd.head}), #sectors=${imd.nsects}, sectsize=${imd.sectsize} ($sectorSize bytes)")

      if (!headerOK(imd)) {
        Utils.outln("IMD: Corrupt header.")
        return
      }

      if ((imd.head + 1) > diskInfo.nsides) {
        diskInfo.nsides = imd.head + 1
      }

      diskInfo.track(imd.cyl)(imd.head).mode = imd.mode
      diskInfo.track(imd.cyl)(imd.head).nsects = imd.nsects
      diskInfo.track(imd.cyl)(imd.head).sectsize = sectorSize

      val cbuf = ByteBuffer.allocate(imd.nsects)
      diskUnit.fileChannel.read(cbuf)
      cbuf.get(sectorMap)
      cbuf.clear()

      if (sectorMap.length != imd.nsects) {
        Utils.outln("IMD: Corrupt file [Sector Map].")
        return
      }
      diskInfo.track(imd.cyl)(imd.head).start_sector = imd.nsects
      Utils.outlnd(diskUnit,"\tSector Map: ")
      for (i <- 0 to imd.nsects) {
        Utils.out(s"${sectorMap(i)} ")
        if (sectorMap(i) < diskInfo.track(imd.cyl)(imd.head).start_sector) {
          diskInfo.track(imd.cyl)(imd.head).start_sector = sectorMap(i)
        }
      }
      Utils.outlnd(diskUnit,s", Start Sector=${diskInfo.track(imd.cyl)(imd.head).start_sector}")

      if ((sectorHeadwithFlags & IMD_FLAG_SECT_HEAD_MAP) != 0) {
        val cbuf = ByteBuffer.allocate(imd.nsects)
        diskUnit.fileChannel.read(cbuf)
        cbuf.get(sectorHeadMap)
        cbuf.clear()

        if (sectorHeadMap.length != imd.nsects) {
          Utils.outln("IMD: Corrupt file [Sector Head Map].")
          return
        }
        Utils.outd(diskUnit, "\tSector Head Map: ")
        for (i <- 0 to imd.nsects) {
          Utils.outd(diskUnit, s"${sectorHeadMap(i)} ");
        }
        Utils.outlnd(diskUnit, " ")
      } else {
        /* Default Head is physical head for each sector */
        for (i <- 0 to imd.nsects) {
          sectorHeadMap(i) = imd.head.byteValue()
        }
      }

      if ((sectorHeadwithFlags & IMD_FLAG_SECT_CYL_MAP) != 0) {
        val cbuf = ByteBuffer.allocate(imd.nsects)
        diskUnit.fileChannel.read(cbuf)
        cbuf.get(sectorCylMap)
        cbuf.clear()
        if (sectorCylMap.length != imd.nsects) {
          Utils.outln("IMD: Corrupt file [Sector Cyl Map].")
          return
        }
        Utils.outd(diskUnit, "\tSector Cyl Map: ")
        for (i <- 0 to imd.nsects) {
          Utils.outd(diskUnit, s"${sectorCylMap(i)} ")
        }
        Utils.outlnd(diskUnit, " ")
      } else {
        /* Default Cyl Map is physical cylinder for each sector */
        for (i <- 0 to imd.nsects) {
          sectorCylMap(i) = imd.cyl.byteValue()
        }
      }

      Utils.outlnd(diskUnit, s"\n\rSector data at offset ${diskUnit.fileChannel.position()}")

      /* Build the table with location 0 being the start sector. */
      start_sect = diskInfo.track(imd.cyl)(imd.head).start_sector

      /* Now read each sector */
      for (i <- 0 to imd.nsects) {
        TotalSectorCount += 1
        Utils.outd(diskUnit, s"Sector Phys: $i/Logical: ${sectorMap(i)}: $sectorSize bytes: ")
        val rbuf = ByteBuffer.allocate(1)
        diskUnit.fileChannel.read(rbuf)
        sectRecordType = rbuf.array()(0)
        rbuf.clear()
        /* AGN Logical head mapping */
        diskInfo.track(imd.cyl)(imd.head).logicalHead(i) = sectorHeadMap(i)
        /* AGN Logical cylinder mapping */
        diskInfo.track(imd.cyl)(imd.head).logicalCyl(i) = sectorCylMap(i)
        sectRecordType match {
          case SECT_RECORD_UNAVAILABLE => /* Data could not be read from the original media */
            if (sectorMap(i) - start_sect < MAX_SPT)
              diskInfo.track(imd.cyl)(imd.head).sectorOffsetMap(sectorMap(i) - start_sect) = 0xBADBAD
            else {
              Utils.outln(s"IMD: ERROR: Illegal sector offset ${sectorMap(i) - start_sect}")
              return //(SCPE_OPENERR);
            }

          case SECT_RECORD_NORM | /* Normal Data */
               SECT_RECORD_NORM_DAM | /* Normal Data with deleted address mark */
               SECT_RECORD_NORM_ERR | /* Normal Data with read error */
               SECT_RECORD_NORM_DAM_ERR => /* Normal Data with deleted address mark with read error */
            /*                  sim_debug(diskInfo.debugmask, diskInfo.device, "Uncompressed Data\n"); */
            if (sectorMap(i) - start_sect < MAX_SPT) {
              diskInfo.track(imd.cyl)(imd.head).sectorOffsetMap(sectorMap(i) - start_sect) = diskUnit.fileChannel.position.intValue()
              diskUnit.fileChannel.position(diskUnit.fileChannel.position + sectorSize)
              //sim_fseek(diskInfo.file, sectorSize, SEEK_CUR);
            }
            else {
              Utils.outln(s"IMD: ERROR: Illegal sector offset ${sectorMap(i) - start_sect}")
              return //(SCPE_OPENERR);
            }

          case SECT_RECORD_NORM_COMP | /* Compressed Normal Data */
               SECT_RECORD_NORM_DAM_COMP | /* Compressed Normal Data with deleted address mark */
               SECT_RECORD_NORM_COMP_ERR | /* Compressed Normal Data */
               SECT_RECORD_NORM_DAM_COMP_ERR => /* Compressed Normal Data with deleted address mark */
            if (sectorMap(i) - start_sect < MAX_SPT) {
              diskInfo.track(imd.cyl)(imd.head).sectorOffsetMap(sectorMap(i) - start_sect) = diskUnit.fileChannel.position().intValue()
              diskUnit.isWriteProtect
              diskUnit.setOption("READONLY", "TRUE", new StringBuilder) //Write-protect the disk if any sectors are compressed.

              val cbuf = ByteBuffer.allocate(1)
              diskUnit.fileChannel.read(cbuf)
              val cdata = cbuf.get(0)

              cbuf.clear()
              Utils.outlnd(diskUnit, s"Compressed Data = $cdata")
            }
            else {
              Utils.outln(s"IMD: ERROR: Illegal sector offset ${sectorMap(i) - start_sect}")
              return // (SCPE_OPENERR);
            }
          case _ =>
            Utils.outln(s"SIM_IMD: ERROR: unrecognized sector record type $sectRecordType")
            return
        }
        Utils.outln(" ")
      }

      diskInfo.ntracks += 1
    }

    Utils.outlnd(diskUnit,s"Processed $TotalSectorCount sectors")

    for (i <- 0 to diskInfo.ntracks) {

      Utils.outd(diskUnit,"Track $i: ")
      for (j <- 0 to imd.nsects) {
        Utils.outd(diskUnit,s"${diskInfo.track(i)(0).sectorOffsetMap(j)} ")
      }
      Utils.outlnd(diskUnit," ")
    }
    if (diskUnit.isWriteProtect) {
      Utils.outln("Disk write-protected because the image contains compressed sectors. Use IMDU to uncompress.")
    }


  }

  /*
   * This function closes the IMD image.  After closing, the sector read/write operations are not
   * possible.
   *
   * Doesn't actually close the fileChannel - we let others do that.
   */
  def diskClose(disk: DiskUnit): Unit = {
    // NO-OP for now
  }


  /*
   * Create an ImageDisk (IMD) file.  This function just creates the comment header.
   * After the IMD is created, it must be formatted with a format
   * program on the simulated operating system, ie CP/M, CDOS, 86-DOS.
   *
   * If the IMD file already exists, it won't be overwritten unless the flag is set.
   */
  def diskCreate(unit: DiskUnit, comment: String, okToOverwrite: Boolean): Unit = {
    if (!unit.isAvailable) {
      return //(SCPE_OPENERR);
    }

    if (unit.fileChannel.size() != 0 && !okToOverwrite) {
      Utils.outln("IMD: Disk image already has data.")
      return
    }

    if (comment.length > MAX_COMMENT_LEN) {
      Utils.outln(s"IMD: Maximum comment size exceeded.  Max = $MAX_COMMENT_LEN")
      return
    }

    /* rewind to the beginning of the file.  Erase the contents */
    unit.fileChannel.truncate(0L)
    unit.fileChannel.position(0)

    val header = s"IMD SIM V1.0\n\r$comment\n\r\n\r${0x1A}"
    val b1 = ByteBuffer.allocate(header.length)
    unit.fileChannel.write(b1)
    unit.fileChannel.force(true)

    //fprintf(fileref, "IMD SIMH %s %s\n", __DATE__, __TIME__);
    //fputs(comment, fileref);
    //free(comment);
    //fprintf(fileref, "\n\n$Id: sim_imd.c 1999 2008-07-22 04:25:28Z hharte $\n");
    //fprintf(fileref, "%s\n", ctlr_comment);
    //fputc(0x1A, fileref); /* EOF marker for IMD comment. */
    //fflush(fileref);

    diskOpen(unit)

    diskFormat(unit)

    diskClose(unit)
  }

  val IMD_MODE_500K_FM = 0
  val IMD_MODE_300K_FM = 1
  val IMD_MODE_250K_FM = 2
  val IMD_MODE_500K_MFM = 3
  val IMD_MODE_300K_MFM = 4
  val IMD_MODE_250K_MFM = 5

  def IMD_MODE_FM(x: Int): Boolean = x <= IMD_MODE_250K_FM

  def IMD_MODE_MFM(x: Int): Boolean = x >= IMD_MODE_500K_MFM

  def diskFormat(unit: DiskUnit): Unit = {
    val sector_map: Array[Byte] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)

    Utils.outln("IMD: Formatting disk in IBM 3740 SS/SD Format.")

    for (i <- 0 to 77) {
      if (trackWrite(unit, i, 0, 26, 128, sector_map, IMD_MODE_500K_FM, 0xE5.byteValue()) != 0) {
        Utils.outln(s"IMD: Error formatting track $i")
        return
      } else {
        Utils.out(".")
      }
    }
    Utils.outln("\n\rIMD: Format Complete.")
  }

  def imdGetSides(myDisk: DiskInfo): Int = {
    if (myDisk != null) {
      myDisk.nsides
    } else 0
  }

  def imdIsWriteLocked(unit: DiskUnit): Boolean = {
    unit.isWriteProtect
  }

  /* Check that the given track/sector exists on the disk */
  def sectSeek(Cyl: Int, Head: Int): Boolean = {
    if (Cyl >= diskInfo.ntracks) {
      return false
    }

    if (Head >= diskInfo.nsides) {
      return false
    }

    if (diskInfo.track(Cyl)(Head).nsects == 0) {
      Utils.outln(s"IMD: (sectSeek) Invalid track/head.")
      return false
    }

    true
  }

  /* Read a sector from an IMD image. */
  def sectRead(unit: DiskUnit, Cyl: Int, Head: Int, Sector: Int, buf: ByteBuffer): (Int, Int) = {
    var readlen = 0
    var flags = 0

    if (!sectSeek(Cyl, Head)) {
      return (IMD_DISK_IO_ERROR_GENERAL, 0)
    }

    if (Sector > diskInfo.track(Cyl)(Head).nsects) {
      Utils.outln(s"IMD: (sectRead) invalid sector: $Sector")
      return (IMD_DISK_IO_ERROR_GENERAL, 0)
    }

    if (buf.capacity() < diskInfo.track(Cyl)(Head).sectsize) {
      Utils.outlnd(unit,s"IMD: (sectRead) Reading C:$Cyl/H:$Head/S:$Sector, len=${buf.capacity()}: user buffer too short, need ${diskInfo.track(Cyl)(Head).sectsize}")
      return (IMD_DISK_IO_ERROR_GENERAL, 0)
    }

    val start_sect = diskInfo.track(Cyl)(Head).start_sector

    val sectorFileOffset = diskInfo.track(Cyl)(Head).sectorOffsetMap(Sector - start_sect)

    Utils.outln(s"IMD: Reading C:$Cyl/H:$Head/S:$Sector, len=${buf.capacity()}, offset=$sectorFileOffset")

    unit.fileChannel.position(sectorFileOffset - 1)

    val sbuf = ByteBuffer.allocate(1)
    unit.fileChannel.read(sbuf)
    val sectRecordType = sbuf.get(0)
    sbuf.clear()
    sectRecordType match {
      case SECT_RECORD_UNAVAILABLE => /* Data could not be read from the original media */
        flags |= IMD_DISK_IO_ERROR_GENERAL
      case SECT_RECORD_NORM_ERR | /* Normal Data with read error */
           SECT_RECORD_NORM_DAM_ERR => /* Normal Data with deleted address mark with read error */
        flags |= IMD_DISK_IO_ERROR_CRC
      case SECT_RECORD_NORM | /* Normal Data */
           SECT_RECORD_NORM_DAM => /* Normal Data with deleted address mark */

        /*          sim_debug(myDisk->debugmask, myDisk->device, "Uncompressed Data\n"); */
        if (unit.fileChannel.read(buf) != diskInfo.track(Cyl)(Head).sectsize) Utils.outln(s"IMD: (sectRead) read error for SECT_RECORD_NORM_DAM.")
        readlen = diskInfo.track(Cyl)(Head).sectsize
      case SECT_RECORD_NORM_COMP_ERR | /* Compressed Normal Data */
           SECT_RECORD_NORM_DAM_COMP_ERR => /* Compressed Normal Data with deleted address mark */
        flags |= IMD_DISK_IO_ERROR_CRC
      case SECT_RECORD_NORM_COMP | /* Compressed Normal Data */
           SECT_RECORD_NORM_DAM_COMP => /* Compressed Normal Data with deleted address mark */
        /*          sim_debug(myDisk->debugmask, myDisk->device, "Compressed Data\n"); */
        val tbuf = ByteBuffer.allocate(1)
        unit.fileChannel.read(tbuf)
        val tchar = tbuf.get(0)
        tbuf.clear()
        for (x <- 0 to diskInfo.track(Cyl)(Head).sectsize) buf.put(tchar)
        readlen = diskInfo.track(Cyl)(Head).sectsize
        flags |= IMD_DISK_IO_COMPRESSED
      case _ =>
        Utils.outln(s"IMD: ERROR - unrecognized sector record type $sectRecordType")
    }

    /* Set flags for deleted address mark. */
    sectRecordType match {
      case SECT_RECORD_NORM_DAM | /* Normal Data with deleted address mark */
           SECT_RECORD_NORM_DAM_ERR | /* Normal Data with deleted address mark with read error */
           SECT_RECORD_NORM_DAM_COMP | /* Compressed Normal Data with deleted address mark */
           SECT_RECORD_NORM_DAM_COMP_ERR => /* Compressed Normal Data with deleted address mark */
        flags |= IMD_DISK_IO_DELETED_ADDR_MARK;
      case _ =>

    }
    (flags, readlen)
  }

  /* Write a sector to an IMD image. */
  def sectWrite(unit: DiskUnit, flags: Int, Cyl: Int, Head: Int, Sector: Int, buf: ByteBuffer): (Int, Int) = {
    Utils.outln("Writing C:$Cyl/H:$Head/S:$Sector, len=$buflen")


    if (!sectSeek(Cyl, Head)) {
      return (IMD_DISK_IO_ERROR_GENERAL, 0)
    }

    if (Sector > diskInfo.track(Cyl)(Head).nsects) {
      Utils.outln("IMD: invalid sector (sectWrite)")
      return (IMD_DISK_IO_ERROR_GENERAL, 0)
    }

    if (unit.isWriteProtect) {
      Utils.outln("Disk write-protected because the image contains compressed sectors. Use IMDU to uncompress.")
      return (IMD_DISK_IO_ERROR_WPROT, 0)
    }

    if (buf.capacity() != diskInfo.track(Cyl)(Head).sectsize) {
      Utils.outln(s"IMD: (sectWrite) user buffer incorrect [buf ${buf.capacity()} != sectsize ${diskInfo.track(Cyl)(Head).sectsize}]")
      return (IMD_DISK_IO_ERROR_GENERAL, 0)
    }

    val start_sect = diskInfo.track(Cyl)(Head).start_sector

    val sectorFileOffset = diskInfo.track(Cyl)(Head).sectorOffsetMap(Sector - start_sect)

    unit.fileChannel.position(sectorFileOffset - 1)

    val sectRecordType = if ((flags & IMD_DISK_IO_ERROR_GENERAL) != 0) {
      SECT_RECORD_UNAVAILABLE
    } else if ((flags & IMD_DISK_IO_ERROR_CRC) != 0) {
      if ((flags & IMD_DISK_IO_DELETED_ADDR_MARK) != 0)
        SECT_RECORD_NORM_DAM_ERR
      else
        SECT_RECORD_NORM_ERR
    } else {
      if ((flags & IMD_DISK_IO_DELETED_ADDR_MARK) != 0)
        SECT_RECORD_NORM_DAM
      else
        SECT_RECORD_NORM
    }

    val stbuf = ByteBuffer.allocate(1)
    stbuf.put(sectRecordType.toByte)
    unit.fileChannel.write(stbuf)
    stbuf.clear()
    //fputc(sectRecordType, myDisk->file);
    val writelen = unit.fileChannel.write(buf)

    (0, writelen)
  }

  /* Format an entire track.  The new track to be formatted must be after any existing tracks on
   * the disk.
   *
   * This routine should be enhanced to re-format an existing track to the same format (this
   * does not involve changing the disk image size.)
   *
   * Any existing data on the disk image will be destroyed when Track 0, Head 0 is formatted.
   * At that time, the IMD file is truncated.  So for the trackWrite to be used to
   * format a disk image, then format program must format tracks starting with Cyl 0, Head 0,
   * and proceed sequentially through all tracks/heads on the disk.
   *
   * Format programs that are known to work include:
   * Cromemco CDOS "INIT.COM"
   * ADC Super-Six (CP/M-80) "FMT8.COM"
   * 86-DOS "INIT.COM"
   *
   */
  def trackWrite(unit: DiskUnit, Cyl: Int, Head: Int, numSectors: Int, sectorLen: Int, sectorMap: Array[Byte], mode: Int, fillbyte: Byte): Int = {
    var track_header: IMD_HEADER = new IMD_HEADER()
    var flags = 0

    if (unit.isWriteProtect) {
      Utils.outln("IMD: Disk write-protected, cannot format tracks.")
      return IMD_DISK_IO_ERROR_WPROT
    }

    Utils.outlnd(unit, s"IMD: Formatting C:$Cyl/H:$Head/N:$numSectors, len=$sectorLen, Fill=$fillbyte")

    /* Truncate the IMD file when formatting Cyl 0, Head 0 */
    if ((Cyl == 0) && (Head == 0)) {
      /* Skip over IMD comment field. */
      commentParse(unit)

      /* Truncate the IMD file after the comment field. */
      unit.fileChannel.truncate(unit.fileChannel.position())

      /* Flush and re-parse the IMD file. */
      unit.fileChannel.force(true)
      diskParse(unit)
    }

    /* Check to make sure the Cyl / Head is not already formatted. */
    if (!sectSeek(Cyl, Head)) {
      Utils.outln(s"IMD: ERROR: Not Formatting C:$Cyl/H:$Head, track already exists.")
      return IMD_DISK_IO_ERROR_GENERAL
    }
    track_header.mode = mode
    track_header.cyl = Cyl
    track_header.head = Head
    track_header.nsects = numSectors
    track_header.sectsize = sectorLen

    /* Forward to end of the file, write track header and sector map. */
    unit.fileChannel.position(unit.fileChannel.size() + 1)
    val imdbuf = ByteBuffer.allocate(5)
    imdbuf.put(track_header.mode.byteValue())
    imdbuf.put(track_header.cyl.byteValue())
    imdbuf.put(track_header.head.byteValue())
    imdbuf.put(track_header.nsects.byteValue())
    imdbuf.put(track_header.sectsize.byteValue())
    unit.fileChannel.write(imdbuf)
    imdbuf.clear()

    //sim_fwrite(& track_header, 1, sizeof(IMD_HEADER), fileref);
    val secbuf = ByteBuffer.allocate(sectorMap.length)
    secbuf.put(sectorMap)
    unit.fileChannel.write(secbuf)
    secbuf.clear()

    //sim_fwrite(sectorMap, 1, numSectors, fileref);

    /* Compute data length, and fill a sector buffer with the
     * sector record type as the first byte, and fill the sector
     * data with the fillbyte.
     */
    val dataLen = sectorLen + 1
    val sectorData = ByteBuffer.allocate(dataLen)
    //memset(sectorData, fillbyte, dataLen);
    sectorData.put(SECT_RECORD_NORM.toByte)
    while (sectorData.hasRemaining) sectorData.put(fillbyte)

    /* For each sector on the track, write the record type and sector data. */
    for (i <- 0 to numSectors) {
      unit.fileChannel.write(sectorData)
      sectorData.rewind()
      //sim_fwrite(sectorData, 1, dataLen, fileref);
    }

    /* Flush the file, and free the sector buffer. */
    unit.fileChannel.force(true)
    sectorData.clear()
    //fflush(fileref);
    //free(sectorData);

    /* Now that the disk track/sector layout has been modified, re-parse the disk image. */
    diskParse(unit)
    0
  }

  /* Utility function to set the image type for a unit to the correct value.
   */
  def assignDiskType(unit: DiskUnit): Unit = {
    val buff = ByteBuffer.allocate(3)
    val pos = unit.fileChannel.position()
    unit.fileChannel.position(0)
    val bytesRead = unit.fileChannel.read(buff)
    if (bytesRead != 3) {
      IMAGE_TYPE = IMAGE_TYPE_DSK
    } else {
      val str = buff.asCharBuffer().toString
      if (str.compareToIgnoreCase("IMD") == 0) IMAGE_TYPE = IMAGE_TYPE_IMD
      else if (str.compareToIgnoreCase("CPT") == 0) IMAGE_TYPE = IMAGE_TYPE_CPT
      else IMAGE_TYPE = IMAGE_TYPE_DSK
    }

    unit.fileChannel.position(pos)
  }


}

class IMD_HEADER(var mode: Int = 0, var cyl: Int = 0, var head: Int = 0, var nsects: Int = 0, var sectsize: Int = 0) {}

//case class TRACK_INFO(mode:UByte, nsects:UByte,sectsize:UInt,sectorOffsetMap:Array[UInt], start_sector:UByte, logicalHead:Array[UByte], logicalCyl:Array[UByte]) {}
//class DISK_INFO(var file: FileChannel, var ntracks:UInt = UInt(0), var nsides:UByte = UByte(0), var unit:DiskUnit = null, var track: Array[Array[TRACK_INFO]] = Array.empty) {}
