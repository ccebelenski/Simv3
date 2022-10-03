package com.sim.pdp11


final case class AbortException(code: Int, private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
