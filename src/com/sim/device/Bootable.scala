package com.sim.device

trait Bootable {

  val supportsBoot = true
  def boot(unitno:Int, sb:StringBuilder): Boolean = {false} // default no op

}
