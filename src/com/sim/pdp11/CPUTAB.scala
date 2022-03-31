package com.sim.pdp11

import com.sim.unsigned.UInt

protected case class CPUTAB(var name: String, // Model name
                            var std: UInt, // standard flags
                            var opt: UInt, // set/clear flags
                            var maxm: UInt, // max memory
                            var psw: UInt, // PSW mask
                            var mfpt: UInt, // MFPT code
                            var par: UInt, // PAR mask
                            var pdr: UInt, // PDR mask
                            var mm0: UInt, // MMR0 mask
                            var mm3: UInt // MMR3 mask
                           ) {
}

