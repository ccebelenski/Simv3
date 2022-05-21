package com.sim

import scala.collection.mutable

class HelpCommand extends Command {
  commandToken = "HELP"
  commandDescription = "Display command help."

  override def process(tokenArray: Array[String]): Boolean = {

    val sb: mutable.StringBuilder = new mutable.StringBuilder

    if (tokenArray.isEmpty) {
      // General help on all commands.

      Console.commandTree.foreach(cmd => {
        sb.append(cmd._2.explain())
      })

      Utils.outln(sb.toString())
    } else {
      Console.commandTree.find(_._2.commandMatch(tokenArray(0))).foreach(cmd => {
        sb.append(cmd._2.commandHelpText)
      })
      Utils.outln(sb.toString())
    }
    false
  }
}
