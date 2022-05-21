package com.sim.device

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait SupportsOptions {

  // unit options - these are copied to each unit created.
  val unitOptions: ArrayBuffer[UnitOption] = new ArrayBuffer[UnitOption]

  def getOption(optionName: String): Option[UnitOption] = {
    unitOptions.find(p => p.optionName == optionName)
  }

  def getValueOption(optionName: String): Option[Int] = {
    val o = unitOptions.find(p => p.optionName == optionName)
    if (o.isDefined) {
      Some(o.get.asInstanceOf[ValueUnitOption].value)
    } else None

  }

  def getBinaryOption(optionName: String): Boolean = {
    val o = unitOptions.find(p => p.optionName == optionName)
    if (o.isDefined) o.get.asInstanceOf[BinaryUnitOption].value
    else false
  }

  def getEnumValueOption(optionName:String): Option[String] = {
    val o = unitOptions.find(p => p.optionName == optionName)
    if(o.isDefined) {
      val z = o.get.asInstanceOf[EnumValueUnitOption].values.find(x => x.value)
      if(z.isDefined) Some(z.get.name) else None
    } else None
  }

  def setOption(optionName: String, optionValue: String, sb: mutable.StringBuilder): Boolean = {
    // First find the option - if we don't find it, then we're done already
    var optionsChanged = false
    try {
      val option = getOption(optionName)
      option match {
        case None => sb.append(s"SIM: Option $optionName is not valid and is ignored.")
        case Some(o: UnitOption) =>
          // Found a valid matching option - now try and set it
          val result = o.setFromString(optionValue, sb)
          if (result) {
            sb.append(s"SIM: ${o.optionName} set: $optionValue\n\r")
            optionsChanged = true
          }
      }
      optionsChanged
    } catch {
      case t: Throwable =>
        sb.append(s"SIM: Option $optionName was not specified correctly and is ignored.\n\r")
        optionsChanged
    }
  }

  // Callback routine when an option has changed.  The device/unit/whatever can decide what to do.
  def optionChanged(sb: mutable.StringBuilder): Unit

}
