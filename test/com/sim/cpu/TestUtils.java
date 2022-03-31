package com.sim.cpu;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.nio.charset.Charset;

public class TestUtils {
    private final static String hexChar = "0123456789ABCDEF";
    public void readHexDumpFile(String fileName, Z80 z80) throws IOException {
        String line;
        Integer address, base;
        LineNumberReader source = new LineNumberReader(new InputStreamReader(new FileInputStream(fileName), Charset.forName("UTF-8")));
        //
        boolean firstTime = true;
        while (true) { // read a line

            String inputLine = source.readLine();
            if ((null == inputLine) || (inputLine.charAt(0) == '.')) {
                break;
            }
            line = inputLine.trim();
            // System.out.println("<" + line + ">");

            // convert and place into memory
            address = getHexValue(line.substring(0, 4));
            //System.out.println("Address : " + address + " : " + line.substring(0, 4));
            if (firstTime) {
                firstTime = false;
            }
            base = 5;
            for (int i = 0; i < 8; i++) {
                Integer value = getHexValue(line.substring(base, base + 2));
                z80.deposit(address,value);
                base = base + 3;
                address++;
            }
        }
        source.close();
    }

    /*
    turn a 4 bit value into its equivalent hex digit
   */
    private static char getHexCharacter(final int value) {
        return hexChar.charAt(value);
    }

    /*
      turn a byte into two hex digits
     */
    private static String getByte(final int value) {

        char[] byteText = new char[2];
        try {
            byteText[0] = getHexCharacter((value >>> 4));
            byteText[1] = getHexCharacter((value & 0x0F));
        } catch (Exception e) {
            byteText[0] = '*';
            byteText[1] = '*';
        }
        return new String(byteText);
    }

    /*
      turn a word into four hex digits
     */
    public static String getWord(final int value) {
        return getByte(value >>> 8) + getByte(value & 0x00FF);
    }

    /*
      convert a hex digit into an int
     */
    public static int getHexDigit(final char hex) {
        if (hex > '9') {
            return hex - 0x37;
        } else {
            return hex - 0x030;
        }
    }

    /*
      convert a hex string into an integer
     */
    public static int getHexValue(String hex) {
        return Integer.parseInt(hex, 16);
    }
}
