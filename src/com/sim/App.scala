package com.sim

import java.util.Properties

import scala.language.existentials
/**
 * Created by christophercebelenski on 7/1/16.
 */
class App {

}

object App {
  def main(args: Array[String]): Unit = {

    //val prop = new Properties()
    //prop.load(classOf[App].getClassLoader.getResourceAsStream("sim.properties"))
    val con: Console = new Console()


    con.initUI()

    //sim.Console.textTerminal.registerUserInterruptHandler((term: (_$1) forSome {type _$1}) => {
    //  sim.Console.userInterrupt = true
    //}, true)


    con.commandLoop()
    System.exit(0)

  }
}



