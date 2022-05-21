package com.sim.device

trait SerialDevice extends SupportsOptions{

  def createSerialUnitOptions(): Unit = {
    unitOptions.append(BinaryUnitOption("ANSI", "Set bit 8 of output to 0", value = false))
    unitOptions.append(BinaryUnitOption("UPPER", "Convert input to upper case", value = false))
    unitOptions.append(BinaryUnitOption("BS", "Map delete to backspace", value = false))
  }
/*
static int32 mapCharacter(int32 ch) {
    ch &= 0xff;
    if (sio_unit.flags & UNIT_SIO_MAP) {
        if (sio_unit.flags & UNIT_SIO_BS) {
            if (ch == BACKSPACE_CHAR)
                return DELETE_CHAR;
        }
        else if (ch == DELETE_CHAR)
            return BACKSPACE_CHAR;
        if (sio_unit.flags & UNIT_SIO_UPPER)
            return toupper(ch);
    }
    return ch;
}
 */

}
