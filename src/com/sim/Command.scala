package com.sim

import scala.collection.mutable

abstract class Command {

  private val subCommandTree: mutable.HashMap[String, Command] = new mutable.HashMap[String, Command]()

  val argsErrorMsg = "?Invalid arguments."

  // Token this command responds to
  var commandToken: String = _
  var commandDescription: String = _
  var commandHelpText: String = "No additional information is available."
  var level: Int = 0

  def process(tokenArray: Array[String]): Boolean


  def addSubCommand(cmd: Command) : Unit= {
    subCommandTree.put(cmd.commandToken, cmd)

  }

  def processSubCommand(tokenArray: Array[String]) : Boolean = {
    val tokenSlice = slice(tokenArray)
    subCommandTree.find(_._2.commandMatch(tokenArray(0))).foreach(cmd => cmd._2.process(tokenSlice))
    false
  }

  def slice(tokenArray: Array[String]) : Array[String] = {
    tokenArray.slice(1,tokenArray.length)
  }

  def commandMatch(token:String):Boolean = {
    if(commandToken.startsWith(token)) true else false
  }

  def help(): String = {
    commandHelpText
  }

  def explain(): String = {
    val sb = new mutable.StringBuilder
    sb.append(commandToken)
    sb.append("\t\t")
    sb.append(commandDescription)
    sb.append("\n\r")

    subCommandTree.foreach(command => {
      explain(command._2,sb)
    })

    sb.toString()
  }

  private def explain(cmd: Command, sb: mutable.StringBuilder): Unit = {

    cmd.level to 0 by -1 foreach(x => sb.append("  "))
    sb.append(cmd.commandToken)
    sb.append("\t\t")
    sb.append(cmd.commandDescription)
    sb.append("\n\r")

    cmd.subCommandTree.foreach(command => {
      explain(command._2,sb)
    })

  }

}
