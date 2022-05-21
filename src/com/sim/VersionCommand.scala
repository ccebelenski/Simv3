package com.sim

class VersionCommand extends Command {
  commandToken = "VERSION"
  commandDescription = "Display simulator version."

  override def process(tokenArray: Array[String]): Boolean = {

    Utils.outln("Sim 0.3 - C. Cebelenski 2017,2020,2022")

    false
  }

}
