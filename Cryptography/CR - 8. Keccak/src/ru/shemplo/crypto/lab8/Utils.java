package ru.shemplo.crypto.lab8;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.StringJoiner;

public class Utils {
    
    public static String readInput () throws IOException {
        var r = new InputStreamReader (System.in);
        var br = new BufferedReader (r);
        
        return br.readLine ();
    }
    
    public static String toHEXString (int [] array) {
        final var sb = new StringJoiner (" ");
        for (int value : array) {
            sb.add (String.format ("%02x", value));
        }
        return sb.toString ();
    }
    
    public static BigInteger concatBytes (int [] bytes, int offsset, int length) {
        var result = BigInteger.ZERO;
        for (int i = 0 ; i < length; i++) {
            final var byteValue = BigInteger.valueOf (bytes [offsset + i]);
            result = result.add (byteValue.shiftLeft (8 * i));
        }
        return result;
    }
    
    public static int [] splitBytes (BigInteger bytes, int [] buffer, int offsset, int length) {
        var mod = BigInteger.valueOf (256);
        
        for (int i = 0 ; i < length; i++) {
            buffer [offsset + i] = bytes.mod (mod).intValue ();
            bytes = bytes.shiftRight (8);
        }
        
        return buffer;
    }
    
    public static final BigInteger MAX_MOD = new BigInteger ("18446744073709551616");
    
    public static BigInteger leftRotate (BigInteger number, int offset, int bits) {
        final var lp = number.shiftRight (bits - (offset % bits));
        final var rp = number.shiftLeft (offset % bits);

        return lp.add (rp).mod (MAX_MOD);
    }
    
}
