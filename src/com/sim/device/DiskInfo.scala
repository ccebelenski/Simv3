package com.sim.device

import com.sim.Utils

class DiskInfo(var ntracks: Int = 0,
                      var nsides: Int = 0,
                      var flags: Int = 0,
                      var debugMask: Int = 0,
                      var verbosedebugmask:Int  = 0,
                      var track: Array[Array[TrackInfo]]  = Array.empty) {

  def verifyDiskInfo() : Unit = {
    if(ntracks < 1) Utils.outln("DiskInfo: WARNING: Number of tracks is 0.")
    if(nsides < 1) {
      Utils.outln("DiskInfo: WARNING: Number of sides is 0")
      return
    }
    for(t:Int  <- 0 to ntracks) {
      for(h:Int  <- 0 to nsides) {
        if(track(t)(h).nsects != track(1)(0).nsects)
          Utils.outln(s"DiskInfo: WARNING: For track $t and head $h expected number of sectors ${track(1)(0).nsects} but got ${track(t)(h).nsects}")
        if(track(t)(h).sectsize != track(1)(0).sectsize)
          Utils.outln(s"DiskInfo: WARNING: For track $t and head $h expected sector size ${track(1)(0).sectsize} but got ${track(t)(h).sectsize}")
        if(track(t)(h).start_sector != track(1)(0).start_sector)
          Utils.outln(s"DiskInfo: WARNING: For track $t and head $h expected start sector ${track(1)(0).start_sector} but got ${track(t)(h).start_sector}")
        
      }
    }

  }
}

class TrackInfo(
                      var mode: Int, var nsects : Int, var sectsize:Int,
                      var sectorOffsetMap: Array[Int], var start_sector:Int,
                      var logicalHead: Array[Int], var logicalCyl: Array[Int]
                    ) {

}
