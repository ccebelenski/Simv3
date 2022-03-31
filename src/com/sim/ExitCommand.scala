package com.sim

class ExitCommand  extends Command {

  commandToken = "EXIT"
  commandDescription = "Exit the simulator."
  commandHelpText = "Exit the simulator, ending the program."

  override def process(tokenArray: Array[String]): Boolean = {

    true
  }
}
