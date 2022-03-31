package com.sim.device

import com.sim.SimException

/**
  * Created by christophercebelenski on 7/19/16.
  */
class UnitException(unit: BasicUnit = null, message: String = null, cause: Throwable = null) extends SimException(message:String,cause:Throwable){

}
