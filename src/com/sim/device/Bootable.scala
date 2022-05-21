package com.sim.device

import scala.collection.mutable

trait Bootable {

  val supportsBoot = true
  def boot(unitno:Int, sb:mutable.StringBuilder): Boolean = {false} // default no op

}
