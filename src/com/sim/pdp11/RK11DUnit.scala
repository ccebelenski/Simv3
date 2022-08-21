package com.sim.pdp11

import com.sim.Utils
import com.sim.device.{BasicUnit, DiskUnit}
import com.sim.s100.S100FD400Device

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{OpenOption, Path, Paths}
import scala.collection.mutable
import java.util
import java.nio.file.StandardOpenOption.{CREATE, READ, SPARSE, WRITE}

class RK11DUnit(device:RK11D) extends BasicUnit(device) with  DiskUnit {

  override val waitTime: Long = 0L
  override var MAX_TRACKS: Int = RK11D.RK_NUMTR
  override var DSK_SECT: Int = RK11D.RK11D.RK_NUMWD * 2 // translate from words to bytes
  override var DSK_SECTSIZE: Int = RK11D.RK_NUMSC

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {}

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

  override def attach(fileSpec: String, sb: mutable.StringBuilder): Boolean = {


    if (isAvailable) {
      sb.append(s"$getName: Unit is still attached.   DETACH first.\n")
      return true
    }

    //  if doesn't exist then assume create a new file
    val p: Path = Paths.get(fileSpec)
    val options = new util.HashSet[OpenOption]
    options.add(SPARSE)
    options.add(CREATE)
    options.add(WRITE)
    options.add(READ)

    // Optionally set up some drive parameters basic on the file.
    setDriveAttributes(p)

    fileChannel = FileChannel.open(p, options)

    // Allocate the bytebuffer
    byteBuffer = ByteBuffer.allocate(DSK_SECTSIZE)


    attachedPath = Some(p)
    capacity = DSK_SECTSIZE * DSK_SECT * MAX_TRACKS
    dirty = false

    sb.append(s"$getName: Attached: ${attachedPath.get.getFileName}\r\n")
    sb.append(s"$getName: Capacity: ${Utils.formatBytes(capacity, false)}\n\r")
    // Attaching enabled the device implicitly
    setEnable(true)
    isAttached = true

    false
  }

  /* Service unit timeout
     If seek in progress, complete seek command
     Else complete data transfer command
     The unit control block contains the function and disk address for
     the current command.
  */

  def rk_svc() : Unit =
  {
    
    var drv:Int = 0
    var err:Int = 0
    var awc:Int = 0
    var wc:Int = 0
    var cma:Int = 0
    var cda:Int = 0
    var t:Int = 0
    var sectsread:Int = 0
    var da:Int = 0
    var cyl:Int = 0
    var track:Int = 0
    var sect:Int = 0
    var ma: Int = 0


    val cpu: PDP11 = device.machine.getCPU.asInstanceOf[PDP11]

    //drv = (int32) (uptr - rk_dev.units); /* get drv number */
    if (this.FUNC == RK11D.RKCS_SEEK) {
      /* seek */
      device.rkcs = device.rkcs | RK11D.RKCS_SCP
      /* set seek done */
      if (device.rkcs & PDP11.CSR_IE) {
        /* ints enabled? */
        rkintq = rkintq | RK_SCPI(drv)
        /* queue request */
        if (device.rkcs & PDP11.CSR_DONE) {
          //sim_debug(RKDEB_INT, & rk_dev, "rk_svc(SET_INT)\n")
          cpu.SET_INT(PDP11.IPL_RK, PDP11.INT_RK) // set int request
        }
      }
      else {
        rkintq = 0
        /* clear queue */
        //sim_debug(RKDEB_INT, & rk_dev, "rk_svc(CLR_INT)\n")
        cpu.CLR_INT(PDP11.IPL_RK, PDP11.INT_RK) /* clr int request */
        /* clear interrupt */
      }
      return //SCPE_OK
    }

    if (!isAttached) {
      /* attached? */
      rk_set_done(RK11D.RKER_DRE)
      return //IORETURN(rk_stopioe, SCPE_UNATT)
    }
    sect = GET_SECT(device.rkda)
    /* get sector, cyl */
    cyl = GET_CYL(device.rkda)
    if (sect >= RK11D.RK_NUMSC) {
      /* bad sector? */
      rk_set_done(RK11D.RKER_NXS)
      return //SCPE_OK
    }
    if (cyl >= RK11D.RK_NUMCY) {
      /* bad cyl? */
      rk_set_done(RK11D.RKER_NXC)
      return //SCPE_OK
    }
    ma = ((device.rkcs & RK11D.RKCS_MEX) << (16 - RK11D.RKCS_V_MEX)) | device.rkba
    /* get mem addr */
    da = GET_DA(device.rkda) * RK11D.RK_NUMWD;
    /* get disk addr */
    wc = 0x10000 - device.rkwc
    /* get wd cnt */
    if ((da + wc) > (int32)  capac) {
      /* overrun? */
      wc =  capac -da
      /* trim transfer */
      device.rker = device.rker | RK11D.RKER_OVR
      /* set overrun err */
    }

    err = 0
    if (wc != 0) {
      /* seek ok? */
      switch(FUNC) {
        /* case on function */

        case RK11D.RKCS_READ: /* read */
          if ((device.rkcs & RK11D.RKCS_FMT)!=0) {
            /* format? */
            cda = da
            for(i:Int  <- 0 until wc) {
              /* fill buffer with cyl #s */
              if (cda >= capac) {
                /* overrun? */
                rker = rker | RKER_OVR
                /* set overrun err */
                wc = i
                /* trim transfer */
                break
              }
              rkxb[i] = (uint16) (((cda / RK11D.RK_NUMWD) / (RK_NUMSF * RK_NUMSC)) << RKDA_V_CYL);
              cda = cda + RK11D.RK_NUMWD;
              /* next sector */
            } /* end for wc */
          } /* end if format */
        else
        {
          /* normal read */
          err = sim_disk_rdsect(uptr, da / RK11D.RK_NUMWD, (uint8 *) rkxb, & sectsread, (wc + RK11D.RK_NUMWD - 1) / RK11D.RK_NUMWD);
          //sim_disk_data_trace(uptr, (uint8 *) rkxb, da / RK11D.RK_NUMWD, sectsread * RK11D.RK_NUMWD * sizeof(* rkxb), "sim_disk_rdsect", RKDEB_DAT & dptr -> dctrl, RKDEB_OPS);
        }
        if (device.rkcs & RK11D.RKCS_INH) {
          /* incr inhibit? */
          if ((t = MAP_WRW(ma, 2, & rkxb[wc - 1])))
          {
            /* store last */
            device.rker = device.rker | RK11D.RKER_NXM
            /* NXM? set flag */
            wc = 0
            /* no transfer */
          }
        }
        else {
          /* normal store */
          if ((t = MAP_WRW(ma, wc << 1, rkxb))) {
            /* store buf */
            rker = rker | RKER_NXM;
            /* NXM? set flag */
            wc = wc - t;
            /* adj wd cnt */
          }
        }
        break
        /* end read */

        case RK11D.RKCS_WRITE: /* write */
        if (device.rkcs & RK11D.RKCS_INH) {
          /* incr inhibit? */
          if ((t = MAP_RDW(ma, 2, & comp))) {
            /* get 1st word */
            rker = rker | RKER_NXM;
            /* NXM? set flag */
            wc = 0;
            /* no transfer */
          }
          for(i:Int <- 0 until wc) // all words same
          rkxb[i] = comp
        }
        else {
          /* normal fetch */
          if ((t = MAP_RDW(ma, wc << 1, rkxb))) {
            /* get buf */
            device.rker = device.rker | RK11D.RKER_NXM
            /* NXM? set flg */
            wc = wc - t
            /* adj wd cnt */
          }
        }
        if (wc != 0) {
          /* any xfer? */
          awc = (wc + (RK11D.RK_NUMWD - 1)) & ~(RK11D.RK_NUMWD - 1)
          /* clr to */
          for(i:Int <- wc until awc) // end of blk
          rkxb[i] = 0;
          //sim_disk_data_trace(uptr, (uint8 *) rkxb, da / RK11D.RK_NUMWD, awc, "sim_disk_wrsect", RKDEB_DAT & dptr -> dctrl, RKDEB_OPS);
          err = sim_disk_wrsect(uptr, da / RK11D.RK_NUMWD, (uint8 *) rkxb, NULL, awc / RK11D.RK_NUMWD);
        }
        break;
        /* end write */

        case RK11D.RKCS_WCHK: /* write check */
          err = sim_disk_rdsect(uptr, da / RK11D.RK_NUMWD, (uint8 *) rkxb, & sectsread, (wc + RK11D.RK_NUMWD - 1) / RK11D.RK_NUMWD);
        //sim_disk_data_trace(uptr, (uint8 *) rkxb, da / RK11D.RK_NUMWD, sectsread * RK11D.RK_NUMWD * sizeof(* rkxb), "sim_disk_rdsect", RKDEB_DAT & dptr -> dctrl, RKDEB_OPS);
        if (err) {
          /* read error? */
          wc = 0
          /* no transfer */
          break
        }
        awc = wc
        /* save wc */
        cma = ma
        for(wc <- 0 until awc)
        {
          cma = ma
          /* loop thru buf */
          if (MAP_RDW(cma, 2, & comp)) {
            /* mem wd */
            device.rker = device.rker | RK11D.RKER_NXM
            /* NXM? set flg */
            break
          }
          if (comp != rkxb[wc]) {
            /* match to disk? */
            device.rker = device.rker | RK11D.RKER_WCE
            /* no, err */
            if ((device.rkcs & RK11D.RKCS_SSE) != 0)
              break
          }
          if (!(device.rkcs & RK11D.RKCS_INH)) /* next mem addr */
            cma = cma + 2;
        } /* end for */
        break;
        /* end wcheck */

        default: /* read check */
          break;
      } /* end switch */
    } /* end else */

    rkwc = (rkwc + wc) & 0xffff
    /* final word count */
    if (!(device.rkcs & RK11D.RKCS_INH)) /* final byte addr */
      ma = ma + (wc << 1);
    rkba = ma & RKBA_IMP;
    /* lower 16b */
    device.rkcs = (device.rkcs & ~RK11D.RKCS_MEX) | ((ma >> (16 - RK11D.RKCS_V_MEX)) & RK11D.RKCS_MEX);
    if ((uptr -> FUNC == RK11D.RKCS_READ) && (device.rkcs & RK11D.RKCS_FMT)) /* read format? */
      da = da + (wc * RK11D.RK_NUMWD); /* count by sectors */
    else da = da + wc + (RK11D.RK_NUMWD - 1);
    /* count by words */
    track = (da / RK11D.RK_NUMWD) / RK11D.RK_NUMSC
    sect = (da / RK11D.RK_NUMWD) % RK_NUMSC;
    rkda = (rkda & RKDA_DRIVE) | (track << RKDA_V_TRACK) | (sect << RKDA_V_SECT);
    rk_set_done(0);

    if (err != 0) {
      /* error? */
      //sim_perror("RK I/O error");
      return
    }
    return //SCPE_OK;
  }
}
