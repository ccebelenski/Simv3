package com.sim.s100

/**
  * Parameters for the different drive types the HDisk driver supports.
  */
abstract class S100HDiskParamsBase {

  // Name of CP/M parameter block
  val name: String

  // Description
  val desc: String

  // capacity
  val capac: Long

  // sectors per track
  val spt: Int

  /* data allocation block shift factor                       */
  val bsh: Int

  /* data allocation block mask                               */
  val blm: Int

  /* extent mask                                              */
  val exm: Int

  /* maximum data block number                                */
  val dsm: Int

  /* total number of directory entries                        */
  val drm: Int

  /* determine reserved directory blocks                      */
  val al0: Int

  /* determine reserved directory blocks                      */
  val al1: Int

  /* size of directory check vector                           */
  val cks: Int

  /* number of reserved tracks                                */
  val off: Int

  /* physical record shift factor, CP/M 3                     */
  val psh: Int

  /* physical record mask, CP/M 3                             */
  val phm: Int

  /* 0 for 128 << psh, > 0 for special                        */
  val physicalSectorSize: Int

  /* offset in physical sector where logical sector starts    */
  val offset: Int

  /* Skew table or None                            */
  val skew: Option[Array[Int]]


  /*
  typedef struct {
    char    name[DPB_NAME_LENGTH + 1];  // name of CP/M disk parameter block                        
    t_addr  capac;                      // capacity                                                 
    uint32  spt;                        // sectors per track                                        
    uint8   bsh;                        // data allocation block shift factor                       
    uint8   blm;                        // data allocation block mask                               
    uint8   exm;                        // extent mask                                              
    uint16  dsm;                        // maximum data block number                                
    uint16  drm;                        // total number of directory entries                        
    uint8   al0;                        // determine reserved directory blocks                      
    uint8   al1;                        // determine reserved directory blocks                      
    uint16  cks;                        // size of directory check vector                           
    uint16  off;                        // number of reserved tracks                                
    uint8   psh;                        // physical record shift factor, CP/M 3                     
    uint8   phm;                        // physical record mask, CP/M 3                             
    int32   physicalSectorSize;         // 0 for 128 << psh, > 0 for special                        
    int32   offset;                     // offset in physical sector where logical sector starts    
    int32   *skew;                      // pointer to skew table or NULL                            
} DPB;
   */

  // Skew tables
  var standard8: Array[Int] = Array(0, 6, 12, 18, 24, 4, 10, 16,
    22, 2, 8, 14, 20, 1, 7, 13,
    19, 25, 5, 11, 17, 23, 3, 9,
    15, 21)

  var apple_ii_DOS: Array[Int] = Array(0, 6, 12, 3, 9, 15, 14, 5,
    11, 2, 8, 7, 13, 4, 10, 1)

  var apple_ii_DOS2: Array[Int] = Array(0, 1, 12, 13, 24, 25, 6, 7,
    18, 19, 30, 31, 28, 29, 10, 11,
    22, 23, 4, 5, 16, 17, 14, 15,
    26, 27, 8, 9, 20, 21, 2, 3)

  var apple_ii_PRODOS: Array[Int] = Array(0, 9, 3, 12, 6, 15, 1, 10,
    4, 13, 7, 8, 2, 11, 5, 14)

  var apple_ii_PRODOS2: Array[Int] = Array(0, 1, 18, 19, 6, 7, 24, 25,
    12, 13, 30, 31, 2, 3, 20, 21,
    8, 9, 26, 27, 14, 15, 16, 17,
    4, 5, 22, 23, 10, 11, 28, 29)

  var mits: Array[Int] = Array(0, 17, 2, 19, 4, 21, 6, 23,
    8, 25, 10, 27, 12, 29, 14, 31,
    16, 1, 18, 3, 20, 5, 22, 7,
    24, 9, 26, 11, 28, 13, 30, 15)

}

// Note in the following CKS = 0 for fixed media which are not supposed to be
// changed while CP/M is executing. Also note that spt (sectors per track) is
// measured in CP/M sectors of size 128 bytes. Standard format "HDSK" must be
// first as index 0 is used as default in some cases.


class HDSK extends S100HDiskParamsBase {
  override val name: String = "HDSK"
  override val desc: String = "AZ80 HDSK"
  override val capac: Long = S100HDSKDevice.HDSK_CAPACITY
  override val spt: Int = 32
  override val bsh: Int = 0x05
  override val blm: Int = 0x1F
  override val exm: Int = 0x01
  override val dsm: Int = 0x07F9
  override val drm: Int = 0x03FF
  override val al0: Int = 0xFF
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0006
  override val psh: Int = 0x00
  override val phm: Int = 0x00
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None
}

class CPM68K extends S100HDiskParamsBase {
  override val name: String = "CPM68K"
  override val desc: String = "CP/M-68K HDSK"
  override val capac: Long =  1 << 24
  override val spt: Int = 1<<17
  override val bsh: Int = 0
  override val blm: Int = 0
  override val exm: Int = 0
  override val dsm: Int = 0
  override val drm: Int = 0
  override val al0: Int = 0
  override val al1: Int = 0
  override val cks: Int = 0
  override val off: Int = 0
  override val psh: Int = 0
  override val phm: Int = 0
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}

class EZ80FL extends S100HDiskParamsBase {
  override val name: String = "EZ80FL"
  override val desc: String = "128K FLASH"
  override val capac: Long = 131072
  override val spt: Int = 32
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 127
  override val drm: Int = 0x003E
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0000
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}

class P112 extends S100HDiskParamsBase {
  override val name: String = "P112"
  override val desc: String = "1.44M P112"
  override val capac: Long = 1474560
  override val spt: Int = 72
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 710
  override val drm: Int = 0x00FE
  override val al0: Int = 0xF0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class SU720 extends S100HDiskParamsBase {
  override val name: String = "SU720"
  override val desc: String = "720K Super I/O"
  override val capac: Long = 737280
  override val spt: Int = 36
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 354
  override val drm: Int = 0x007E
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0020
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class OSB1 extends S100HDiskParamsBase {
  override val name: String = "OSB1"
  override val desc: String = "Osborne1 5.25 SS SD"
  override val capac: Long = 102400
  override val spt: Int = 20
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x01
  override val dsm: Int = 45
  override val drm: Int = 0x003F
  override val al0: Int = 0x80
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0003
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class OSB2 extends S100HDiskParamsBase {
  override val name: String = "OSB2"
  override val desc: String = "Osborne1 5.25 SS DD"
  override val capac: Long = 204800
  override val spt: Int = 40
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 184
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0003
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class NSSS1 extends S100HDiskParamsBase {
  override val name: String = "NSSS1"
  override val desc: String = "Northstar SSDD Format 1"
  override val capac: Long = 179200
  override val spt: Int = 40
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 0xA4
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0010
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class NSSS2 extends S100HDiskParamsBase {
  override val name: String = "NSSS2"
  override val desc: String = "Northstar SSDD Format 2"
  override val capac: Long = 179200
  override val spt: Int = 40
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x01
  override val dsm: Int = 0x51
  override val drm: Int = 0x003F
  override val al0: Int = 0x80
  override val al1: Int = 0x00
  override val cks: Int = 0x0010
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class NSDS2 extends S100HDiskParamsBase {
  override val name: String = "NSDS2"
  override val desc: String = "Northstar DSDD Format 2"
  override val capac: Long = 358400
  override val spt: Int = 40
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x01
  override val dsm: Int = 0xA3
  override val drm: Int = 0x003F
  override val al0: Int = 0x80
  override val al1: Int = 0x00
  override val cks: Int = 0x0010
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}

class VGSS extends S100HDiskParamsBase {
  override val name: String = "VGSS"
  override val desc: String = "Vector SS SD"
  override val capac: Long = 315392
  override val spt: Int = 32
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 149
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0020
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class VGDS extends S100HDiskParamsBase {
  override val name: String = "VGDS"
  override val desc: String = "Vector DS SD"
  override val capac: Long = 630784
  override val spt: Int = 32
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 299
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0020
  override val off: Int = 0x0004
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class DISK1A extends S100HDiskParamsBase {
  override val name: String = "DISK1A"
  override val desc: String = "CompuPro Disk1A 8 SS SD"
  override val capac: Long = 630784
  override val spt: Int = 64
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 299
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0020
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class SSSD8 extends S100HDiskParamsBase {
  override val name: String = "SSSD8"
  override val desc: String = "Standard 8 SS SD"
  override val capac: Long = 256256
  override val spt: Int = S100HDSKDevice.SPT26
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 242
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x00
  override val phm: Int = 0x00
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class SSSD8S extends S100HDiskParamsBase {
  override val name: String = "SSSD8S"
  override val desc: String = "Standard 8 SS SD with skew"
  override val capac: Long = 256256
  override val spt: Int = S100HDSKDevice.SPT26
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 242
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x00
  override val phm: Int = 0x00
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = Some(standard8)

}

class SSDD8 extends S100HDiskParamsBase {
  override val name: String =  "SSDD8"
  override val desc: String = "Standard 8 SS DD"
  override val capac: Long = 512512
  override val spt: Int = S100HDSKDevice.SPT52
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x01
  override val dsm: Int = 242
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x01
  override val phm: Int = 0x01
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class SSDD8S extends S100HDiskParamsBase {
  override val name: String = "SSDD8S"
  override val desc: String = "Standard 8 SS DD with skew"
  override val capac: Long = 512512
  override val spt: Int = S100HDSKDevice.SPT52
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x01
  override val dsm: Int = 242
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x01
  override val phm: Int = 0x01
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = Some(standard8)

}
class DSDD8 extends S100HDiskParamsBase {
  override val name: String = "DSDD8"
  override val desc: String = "Standard 8 DS DD"
  override val capac: Long = 1025024
  override val spt: Int = S100HDSKDevice.SPT52
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 493
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x01
  override val phm: Int = 0x01
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class DSDD8S extends S100HDiskParamsBase {
  override val name: String = "DSDD8S"
  override val desc: String = "Standard 8 DS DD with skew"
  override val capac: Long = 1025024
  override val spt: Int = S100HDSKDevice.SPT52
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 493
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x01
  override val phm: Int = 0x01
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class D512SSDD8 extends S100HDiskParamsBase {
  override val name: String = "512SSDD8"
  override val desc: String = "Standard 8 SS DD with 512 byte sectors"
  override val capac: Long = 591360
  override val spt: Int = 60
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 280
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class D512DSDD8 extends S100HDiskParamsBase {
  override val name: String = "512DSDD8"
  override val desc: String = "Standard 8 DS DD with 512 byte sectors"
  override val capac: Long = 1182720
  override val spt: Int = 60
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 569
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
class APPLEDO extends S100HDiskParamsBase {
  override val name: String = "APPLE-DO"
  override val desc: String = "Apple II DOS 3.3"
  override val capac: Long = 143360
  override val spt: Int = S100HDSKDevice.SPT32
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 127
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0003
  override val psh: Int = 0x01
  override val phm: Int = 0x01
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = Some(apple_ii_DOS)

}
class APPLEPO extends S100HDiskParamsBase {
  override val name: String = "APPLE-PO"
  override val desc: String = "Apple II PRODOS"
  override val capac: Long = 143360
  override val spt: Int = S100HDSKDevice.SPT32
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 127
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0003
  override val psh: Int = 0x01
  override val phm: Int = 0x01
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = Some(apple_ii_PRODOS)

}
class APPLED2 extends S100HDiskParamsBase {
  override val name: String = "APPLE-D2"
  override val desc: String = "Apple II DOS 3.3, deblocked"
  override val capac: Long = 143360
  override val spt: Int = S100HDSKDevice.SPT32
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 127
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x003
  override val psh: Int = 0x00
  override val phm: Int = 0x00
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = Some(apple_ii_DOS2)

}
class APPLEP2 extends S100HDiskParamsBase {
  override val name: String = "APPLE-P2"
  override val desc: String = "Apple II PRODOS, deblocked"
  override val capac: Long = 143360
  override val spt: Int = S100HDSKDevice.SPT32
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 127
  override val drm: Int = 0x003F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0003
  override val psh: Int = 0x00
  override val phm: Int = 0x00
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = Some(apple_ii_PRODOS2)

}
class MITS extends S100HDiskParamsBase {
  override val name: String = "MITS"
  override val desc: String = "MITS Altair original"
  override val capac: Long = 337568
  override val spt: Int = S100HDSKDevice.SPT32
  override val bsh: Int = 0x03
  override val blm: Int = 0x07
  override val exm: Int = 0x00
  override val dsm: Int = 254
  override val drm: Int = 0x00FF
  override val al0: Int = 0xFF
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0006
  override val psh: Int = 0x00
  override val phm: Int = 0x00
  override val physicalSectorSize: Int = 137
  override val offset: Int = 3
  override val skew: Option[Array[Int]] = Some(mits)

}
class MITS2 extends S100HDiskParamsBase {
  override val name: String = "MITS2"
  override val desc: String = "MITS Altair original, extra"
  override val capac: Long = 1113536
  override val spt: Int = S100HDSKDevice.SPT32
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x00
  override val dsm: Int = 0x1EF
  override val drm: Int = 0x00FF
  override val al0: Int = 0xF0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0006
  override val psh: Int = 0x00
  override val phm: Int = 0x00
  override val physicalSectorSize: Int = 137
  override val offset: Int = 3
  override val skew: Option[Array[Int]] = Some(mits)

}
class V1050 extends S100HDiskParamsBase {
  //
  //dw     40              ;#128 byte records/track
  //db     4,0fh           ;block shift mask (2K)
  //db     1               ;extent  mask
  //dw     194             ;maximum  block number
  //dw     127             ;max number of dir entry - 1
  //db     0C0H,00h        ;alloc vector for directory
  //dw     0020h           ;checksum size
  //dw     2               ;offset for sys tracks
  //db     2,3             ;physical sector shift (512 sector)

  override val name: String =  "V1050"
  override val desc: String = "Visual Technology Visual 1050"
  override val capac: Long = 409600
  override val spt: Int = 40
  override val bsh: Int = 0x04
  override val blm: Int = 0x0F
  override val exm: Int = 0x01
  override val dsm: Int = 194
  override val drm: Int = 0x007F
  override val al0: Int = 0xC0
  override val al1: Int = 0x00
  override val cks: Int = 0x0000
  override val off: Int = 0x0002
  override val psh: Int = 0x02
  override val phm: Int = 0x03
  override val physicalSectorSize: Int = 0
  override val offset: Int = 0
  override val skew: Option[Array[Int]] = None

}
