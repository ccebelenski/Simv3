package com.sim

import com.sim.term.Term

import scala.collection.mutable
import com.sim.term.cli.SimCLI

class Console {

  def initUI(): Unit = {

    Console.cli = new SimCLI
    Console.cli.setPrompt("SIM>")
    Console.term = Console.cli.getTerm


    // Add the "top level" simulator commands - VERSION, HELP, LOAD, EXIT, SHOW, SET, ATTACH, DETACH, etc.
    val version: Command = new VersionCommand
    Console.commandTree.put(version.commandToken, version)

    val help: Command = new HelpCommand
    Console.commandTree.put(help.commandToken, help)

    val list: Command = new ListCommand
    Console.commandTree.put(list.commandToken, list)

    val show: Command = new ShowCommand
    Console.commandTree.put(show.commandToken, show)

    val set: Command = new SetCommand
    Console.commandTree.put(set.commandToken, set)

    val em: Command = new EM
    Console.commandTree.put(em.commandToken, em)

    val er: Command = new ER
    Console.commandTree.put(er.commandToken, er)

    val exit: Command = new ExitCommand
    Console.commandTree.put(exit.commandToken, exit)

    val boot: Command = new BootCommand
    Console.commandTree.put(boot.commandToken, boot)

    val attach: Command = new AttachCommand
    Console.commandTree.put(attach.commandToken, attach)

    val disasm: DisassembleCommand = new DisassembleCommand
    Console.commandTree.put(disasm.commandToken, disasm)

    val detach: Command = new DetachCommand
    Console.commandTree.put(detach.commandToken, detach)

    val go: GoCommand = new GoCommand
    Console.commandTree.put(go.commandToken, go)

    val step: StepCommand = new StepCommand
    Console.commandTree.put(step.commandToken, step)

    val halt: HaltCommand = new HaltCommand
    Console.commandTree.put(halt.commandToken, halt)

    val break = new BreakCommand
    val unbreak = new UnBreakCommand
    Console.commandTree.put(break.commandToken, break)
    Console.commandTree.put(unbreak.commandToken, unbreak)

    val mlog = new MLogCommand
    val unmlog = new UnMLogCommand
    Console.commandTree.put(mlog.commandToken, mlog)
    Console.commandTree.put(unmlog.commandToken, unmlog)


    Utils.outln("\n\r")
    version.process(null)
  }

  private def readCommand(): String = {
    Console.cli.getline

  }

  def commandLoop(): Unit = {
    var exiting: Boolean = false
    while (!exiting) {
      //Console.userInterrupt = false // Reset the interrupt
      val cmd = readCommand()
      exiting = evalCommand(cmd.trim)
    }
  }

  private def evalCommand(cmd: String): Boolean = {

    if (cmd == null || cmd.isEmpty) return false
    val cmdTokenList = cmd.toUpperCase.split(' ')

    Console.commandTree.find(_._2.commandMatch(cmdTokenList(0))) match {
      case None =>
        Utils.outln("SIM: Invalid command")
        false
      case Some(x) =>
        x._2.process(cmdTokenList.slice(1, cmdTokenList.length))
    }
  }
}

object Console {

  var cli: SimCLI = _
  var term: Term = _

  @volatile
  var cpuRunning:Boolean = false

  val commandTree: mutable.HashMap[String, Command] = new mutable.HashMap[String, Command]()

  val simEnvironment: SimEnvironment = new SimEnvironment

  // This is set by the user interrupt - should be re-set when the command prompt is displayed.
  @volatile
  var userInterrupt: Boolean = false
}
