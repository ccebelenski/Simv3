package com.sim.device

import java.nio.file.Path
import scala.collection.mutable

trait UnitAttachable {

  def attach(filespec: String, sb: mutable.StringBuilder): Boolean

  def detach(sb: mutable.StringBuilder): Boolean

  // Attached path
  var attachedPath: Option[Path] = None
  // Does this unit support being attached?
  val supportsAttach: Boolean = true

  def showAttachedInfo(sb: mutable.StringBuilder): Unit = {

    if (supportsAttach && attachedPath.isDefined) sb.append(s"Attached: ${attachedPath.get.getFileName.toString}\n\r")
  }

}
