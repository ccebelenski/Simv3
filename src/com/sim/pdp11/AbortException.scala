package com.sim.pdp11

import com.sim.unsigned.UInt


final case class AbortException(code: Int, private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
