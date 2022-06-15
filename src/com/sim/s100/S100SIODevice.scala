package com.sim.s100

import com.sim.Utils
import com.sim.cpu.Z80MMU
import com.sim.device.{BinaryUnitOption, PortMappedDevice, SerialDevice, ValueUnitOption}
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable

/**
  * A simulated MITS 2SIO interface card.
  *
  * The card had two physical I/O ports which could be connected
  * to any serial I/O device that would connect to a current loop,
  * RS232, or TTY interface. Available baud rates were jumper
  * selectable for each port from 110 to 9600.
  * All I/O is via programmed I/O. Each device has a status port
  * and a data port. A write to the status port can select
  * some options for the device (0x03 will reset the port).
  * A read of the status port gets the port status:
  * +---+---+---+---+---+---+---+---+
  * | X | X | X | X | X | X | O | I |
  * +---+---+---+---+---+---+---+---+
  * I - A 1 in this bit position means a character has been received
  * on the data port and is ready to be read.
  * O - A 1 in this bit means the port is ready to receive a character
  * on the data port and transmit it out over the serial line.
  * A read to the data port gets the buffered character, a write
  * to the data port writes the character to the device.
  *
  * @param machine s100
  * @param mmu mmu
  * @param ports List of ports this unit responds to
  */
class S100SIODevice(machine: S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDevice(machine, mmu, ports) with SerialDevice {

  // Flags for read/write status
  val CAN_READ: UByte = UByte(1)
  val CAN_WRITE: UByte = UByte(2)


  override val description: String = "MITS 2SIO interface card"
  override val name = "SIO"

  // We only have one unit, the main serial port.  Because of this we can take some shortcuts.
  var SIOUnit: S100SIOUnit = _

  debug = false

  override def init(): Unit = {

    // Create a default serial console unit
    SIOUnit = new S100SIOUnit(this)
    addUnit(SIOUnit)

    // Register this device with the default MUX
    //    machine.findDevice("MUXA") match {
    //      case None => Utils.outln(s"$getName: Could not register device with MUXA.")
    //      case Some(x:MuxDevice) =>
    //        x.registerDevice(this)
    //        this.registeredMuxDevice = Some(x)
    //      case _ => Utils.outln(s"$getName: Misconfiguration - register device with MUXA.")
    //    }

  }

  override def showCommand(sb: mutable.StringBuilder): Unit = {
    super.showCommand(sb)

  }

  override def createUnitOptions(): Unit = {
    createSerialUnitOptions()

    unitOptions.append(BinaryUnitOption("INTERRUPT", "Status port 0 creates an interrupt when a character becomes available", value = false))
    unitOptions.append(ValueUnitOption("IOPORT", "Set I/O port to IOPORT", value = 0))

  }

  //  // Only one unit, so we take some shortcuts...
  //  override def muxCharacterInterrupt(unit: MuxUnit, char: Int): Unit = {
  //    if(!SIOUnit.inputCharacterWaiting) {
  //      mmu.cpu.keyboardInterrupt = false
  //      SIOUnit.inputCharacter = char
  //      SIOUnit.inputCharacterWaiting = true
  //      //machine.eventQueue.activate(SIOUnit, SIOUnit.waitTime)
  //    }
  //  }

  //  // Returns true when the interrupt flag for keyboard is not set.
  //  override def checkDeviceReady: Boolean = {
  //    !mmu.cpu.keyboardInterrupt
  //  }

  def interruptOff(): Unit = {
    mmu.cpu.keyboardInterrupt = false
    //machine.eventQueue.cancel(SIOUnit)
  }

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {
    //    val ch = value.toChar
    //    Utils.outln(s"$getName: Port: $action Value:$value Char: $ch isWrite: $isWrite")

    // Port 10 is status port for console terminal
    if (action == 0x10) {

      if (!isWrite) {
        if (SIOUnit.inputCharacterWaiting) {
          //Utils.outln(s"$getName: CANREAD | CANWRITE")
          return UByte((CAN_READ | CAN_WRITE).byteValue())
        } else {
          //Utils.outln(s"$getName: None")
          return CAN_WRITE
        }
      }

      UByte(0) // writes are ignored - nothing to reset anyway.

    } else if (action == 0x11) {

      if (isWrite) {
        //        if(value.intValue() < 10 || value.intValue() > 125)
        //          {
        Utils.outlnd(this, s"Char: ${value.intValue()}\t: ${value.byteValue.toChar}")
        //          }
        SIOUnit.getTerminal.print(s"${value.byteValue.toChar}")
        UByte(0)
      } else {
        // read char
        val b: Byte = SIOUnit.inputBuffer.dequeue()
        if (SIOUnit.inputBuffer.isEmpty) {

          SIOUnit.inputCharacterWaiting = false
        }
        //Utils.outln(b.toString)
        interruptOff()
        UByte(b)
      }

    } else if (action == 0x12) {
      if (isWrite) UByte(0) // ignore
      else CAN_WRITE
    } else {
      Utils.outlnd(this, s"$getName: Misconfiguration, write to port ${action.toHexString} value ${value.toHexString}")
      UByte(0)
    }
  }

}

/*
static SIO_PORT_INFO port_table[PORT_TABLE_SIZE] = {
    {0x00, 0, KBD_HAS_CHAR,     KBD_HAS_NO_CHAR, SIO_CAN_WRITE, FALSE, 0, FALSE, TRUE   },
    {0x01, 0, 0,                0,      0, FALSE, 0, FALSE, TRUE                        },
    {0x02, 0, VGSIO_CAN_READ,   0,      VGSIO_CAN_WRITE, FALSE, 0, TRUE, TRUE           },
    {0x03, 0, VGSIO_CAN_READ,   0,      VGSIO_CAN_WRITE, FALSE, 0, FALSE, TRUE          },
    {0x10, 0, SIO_CAN_READ,     0,      SIO_CAN_WRITE, TRUE, SIO_RESET, FALSE, TRUE     },
    {0x11, 0, SIO_CAN_READ,     0,      SIO_CAN_WRITE, TRUE, SIO_RESET, TRUE, TRUE      },
    {0x14, 1, SIO_CAN_READ,     0,      SIO_CAN_WRITE, TRUE, SIO_RESET, FALSE, TRUE     },
    {0x15, 1, SIO_CAN_READ,     0,      SIO_CAN_WRITE, TRUE, SIO_RESET, TRUE, TRUE      },
    {0x16, 2, SIO_CAN_READ,     0,      SIO_CAN_WRITE, TRUE, SIO_RESET, FALSE, TRUE     },
    {0x17, 2, SIO_CAN_READ,     0,      SIO_CAN_WRITE, TRUE, SIO_RESET, TRUE, TRUE      },
    {0x18, 3, SIO_CAN_READ,     0,      SIO_CAN_WRITE, TRUE, SIO_RESET, FALSE, TRUE     },
{0x19, 3, SIO_CAN_READ, 0, SIO_CAN_WRITE, TRUE, SIO_RESET, TRUE, TRUE },

typedef struct {
    int32 port;             /* this information belongs to port number 'port'           */
    int32 terminalLine;     /* map to this 'terminalLine'                               */
    int32 sio_can_read;     /* bit mask to indicate that one can read from this port    */
    int32 sio_cannot_read;  /* bit mask to indicate that one cannot read from this port */
    int32 sio_can_write;    /* bit mask to indicate that one can write to this port     */
    int32 hasReset;         /* TRUE iff SIO has reset command                           */
    int32 sio_reset;        /* reset command                                            */
    int32 hasOUT;           /* TRUE iff port supports OUT command                       */
    int32 isBuiltin;        /* TRUE iff mapping is built in                             */
} SIO_PORT_INFO;

#define SIO_CAN_READ        0x01                /* bit 0 is set iff character available         */
#define SIO_CAN_WRITE       0x02                /* bit 1 is set iff character can be sent       */
#define SIO_RESET           0x03                /* Command to reset SIO                         */
#define VGSIO_CAN_READ      0x02                /* bit 1 is set iff character available         */
#define VGSIO_CAN_WRITE     0x01                /* bit 0 is set iff character can be sent       */
#define KBD_HAS_CHAR        0x40                /* bit 6 is set iff character available         */
#define KBD_HAS_NO_CHAR 0x01 /* bit 0 is set iff no character is available   */

/* reset routines */
static t_stat sio_reset(DEVICE *dptr) {
//??  Maybe reset the mux unit?  Not sure

}

static t_stat ptr_reset(DEVICE *dptr) {
    sim_debug(VERBOSE_MSG, &ptr_dev, "PTR: " ADDRESS_FORMAT " Reset\n", PCX);
    resetSIOWarningFlags();
    ptr_unit.u3 = FALSE;                                    /* End Of File not yet reached              */
    ptr_unit.buf = 0;
    if (ptr_unit.flags & UNIT_ATT)                          /* attached?                                */
        rewind(ptr_unit.fileref);
    sim_map_resource(0x12, 1, RESOURCE_TYPE_IO, &sio1s, dptr->flags & DEV_DIS);
    sim_map_resource(0x13, 1, RESOURCE_TYPE_IO, &sio1d, dptr->flags & DEV_DIS);
    return SCPE_OK;
}

static t_stat ptp_reset(DEVICE *dptr) {
    sim_debug(VERBOSE_MSG, &ptp_dev, "PTP: " ADDRESS_FORMAT " Reset\n", PCX);
    resetSIOWarningFlags();
    sim_map_resource(0x12, 1, RESOURCE_TYPE_IO, &sio1s, dptr->flags & DEV_DIS);
    sim_map_resource(0x13, 1, RESOURCE_TYPE_IO, &sio1d, dptr->flags & DEV_DIS);
    return SCPE_OK;
}




/* generic status port for keyboard input / terminal output */
static int32 sio0sCore(const int32 port, const int32 io, const int32 data) {
    int32 ch, result;
[...]
    if (io == 0) { /* IN */
        [...]
[...]
        if (sio_unit.u3)                                    /* character available?                     */
            return spi.sio_can_read | spi.sio_can_write;
[...]
[...]
      return spi.sio_cannot_read | spi.sio_can_write;
    }                                                       /* OUT follows, no fall-through from IN     */
[reset]
    return 0x00;                                            /* ignored since OUT                        */
}

int32 sio0s(const int32 port, const int32 io, const int32 data) {
    const int32 result = sio0sCore(port, io, data);
[...]
    return result;
}

/* generic data port for keyboard input / terminal output */
static int32 sio0dCore(const int32 port, const int32 io, const int32 data) {
[... poll]
    if (io == 0) { /* IN */
        if ((sio_unit.flags & UNIT_ATT) && (!sio_unit.u4))
            return mapCharacter(tmxr_getc_ln(&TerminalLines[spi.terminalLine]));
        if (!sio_unit.u3) {
            sim_debug(BUFFER_EMPTY_MSG, &sio_dev, "\tSIO_D: " ADDRESS_FORMAT
                      " IN(0x%03x) for empty character buffer\n", PCX, port);
        }
        sio_unit.u3 = FALSE;                                /* no character is available any more       */
        return mapCharacter(sio_unit.buf);                  /* return previous character                */
    }                                                       /* OUT follows, no fall-through from IN     */
    if (spi.hasOUT) {
        ch = sio_unit.flags & UNIT_SIO_ANSI ? data & 0x7f : data;   /* clear highest bit in ANSI mode   */
        if ((ch != CONTROLG_CHAR) || !(sio_unit.flags & UNIT_SIO_BELL)) {
            voidSleep();
            if ((sio_unit.flags & UNIT_ATT) && (!sio_unit.u4)) {    /* attached to a port and not to a file */
                tmxr_putc_ln(&TerminalLines[spi.terminalLine], ch); /* status ignored                   */
                tmxr_poll_tx(&altairTMXR);                          /* poll xmt                         */
            }
            else
                sim_putchar(ch);
        }
    }
    return 0x00;                                            /* ignored since OUT                        */
}


int32 sio0d(const int32 port, const int32 io, const int32 data) {
    char buffer[8];
    const int32 result = sio0dCore(port, io, data);
    return result;
}



static t_stat sio_svc(UNIT *uptr) {
    if (sio0s(0, 0, 0) & KBD_HAS_CHAR)
        keyboardInterrupt = TRUE;
    if (sio_unit.flags & UNIT_SIO_INTERRUPT)
        sim_activate(&sio_unit, sio_unit.wait);             /* activate unit    */
    return SCPE_OK;
}

 */