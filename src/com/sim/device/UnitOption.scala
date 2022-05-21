package com.sim.device

import scala.collection.mutable

abstract class UnitOption(val optionName: String, val optionDescription: String) {

  def copy: UnitOption

  def formatValue: String

  def showOption(sb: mutable.StringBuilder): Unit = {
    sb.append(s"  Option: ${optionName.toUpperCase} \t\t$optionDescription\t$formatValue\n\r")
  }

  def setFromString(s: String, sb: mutable.StringBuilder): Boolean

  def optionHelp: String
}

/**
 * Binary Unit Options
 *
 * @param optionName
 * @param optionDescription
 * @param value
 */
case class BinaryUnitOption(override val optionName: String, override val optionDescription: String, var value: Boolean) extends UnitOption(optionName: String, optionDescription: String) {
  def getValue: Boolean = value

  override def formatValue: String = s"${if (!value) "NO" else ""}${optionName.toUpperCase}"

  override def copy: BinaryUnitOption = {
    BinaryUnitOption(optionName, optionDescription, value)
  }

  override def setFromString(s: String, sb: mutable.StringBuilder): Boolean = {
    try {
      value = s.toBoolean
      true
    } catch {
      case t: Throwable =>
        sb.append(optionHelp)
        false
    }
  }

  override def optionHelp: String = s"${optionName.toUpperCase} is binary.  Valid values are TRUE and FALSE."
}

/**
 * Value unit options - Int
 *
 * @param optionName
 * @param optionDescription
 * @param value
 */
case class ValueUnitOption(override val optionName: String, override val optionDescription: String, var value: Int) extends UnitOption(optionName: String, optionDescription: String) {
  def getValue: Int = value

  override def copy: ValueUnitOption = {
    ValueUnitOption(optionName, optionDescription, value)
  }

  override def setFromString(s: String, sb: mutable.StringBuilder): Boolean = {
    try {
      value = s.toInt
      true
    } catch {
      case t: Throwable =>
        sb.append(optionHelp)
        false
    }
  }

  override def optionHelp: String = s"${optionName.toUpperCase} is value.  Valid values are integer numbers."

  override def formatValue: String = s"$value"
}

/**
 * Enum type unit options - only one can be set at time.
 *
 * @param name
 * @param value
 */
case class UnitOptionValue(var name: String, var value: Boolean)

case class EnumValueUnitOption(override val optionName: String,
                               override val optionDescription: String, values: List[UnitOptionValue])
  extends UnitOption(optionName: String, optionDescription: String) {

  override def copy: EnumValueUnitOption = {
    EnumValueUnitOption(optionName, optionDescription, values)
  }

  override def optionHelp: String = {
    val sb = new mutable.StringBuilder
    sb.append(s"${optionName.toUpperCase} is value.  Valid values are:\n\r")
    values.foreach(x => sb.append(s"${x.name}\n\r"))

    sb.toString()
  }

  override def setFromString(s: String, sb: mutable.StringBuilder): Boolean = {
    val current = values.find(v => v.value)
    val option = values.find(v => v.name == s)
    if (option.isDefined) {
      if (current.isDefined) current.get.value = false
      option.get.value = true
      true
    } else false
  }

  override def formatValue: String = s"[${values.toString}]"
}

/**
 * Range unit options - value must be within the range.
 *
 * @param optionName
 * @param optionDescription
 * @param lowValue
 * @param highValue
 * @param currentValue
 */
case class RangeValueUnitOption(override val optionName: String, override val optionDescription: String, lowValue: Int, highValue: Int, var currentValue: Int) extends UnitOption(optionName: String, optionDescription: String) {
  override def copy: RangeValueUnitOption = {
    RangeValueUnitOption(optionName, optionDescription, lowValue, highValue, currentValue)
  }

  override def setFromString(s: String, sb: mutable.StringBuilder): Boolean = ???

  override def optionHelp: String = s"${optionName.toUpperCase} is value.  Valid values are integer numbers in range $lowValue to $highValue"

  override def formatValue: String = s"$lowValue to $highValue"
}

/**
 * Value unit option - String
 *
 * @param optionName
 * @param optionDescription
 * @param value
 */
case class StringValueUnitOption(override val optionName: String, override val optionDescription: String, var value: String) extends UnitOption(optionName, optionDescription) {
  override def copy: StringValueUnitOption = {
    StringValueUnitOption(optionName, optionDescription, value)
  }

  override def setFromString(s: String, sb: mutable.StringBuilder): Boolean = {
    try {
      value = s
      true
    } catch {
      case t: Throwable =>
        sb.append(optionHelp)
        false
    }
  }

  override def optionHelp: String = s"${optionName.toUpperCase} is value.  Valid values are a string."

  override def formatValue: String = s"$value"

}
