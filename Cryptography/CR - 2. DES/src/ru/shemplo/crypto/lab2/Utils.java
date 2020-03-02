package ru.shemplo.crypto.lab2;

import static ru.shemplo.crypto.lab2.Permutations.permuteE;
import static ru.shemplo.crypto.lab2.Permutations.permuteP;
import static ru.shemplo.crypto.lab2.Permutations.permutePC1;
import static ru.shemplo.crypto.lab2.Permutations.permutePC2;
import static ru.shemplo.crypto.lab2.Permutations.permuteS;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Utils {
    
    public static String readInput () throws IOException {
        var r = new InputStreamReader (System.in);
        var br = new BufferedReader (r);
        
        return br.readLine ();
    }
    
    public static long getLongFromBytes (byte [] array, int offset) {
        long l = 0;
        for (int i = 0; i < 8; i++) {
            byte value;
            if ( (offset + i) < array.length) {
                value = array [offset + i];
            } else {
                value = 0;
            }
            l = l << 8 | (value & 0xFFL);
        }
        return l;
    }
    
    public static void getBytesFromLong (byte [] array, int offset, long l) {
        for (int i = 7; i > -1; i--) {
            if ( (offset + i) < array.length) {
                array [offset + i] = (byte) (l & 0xFF);
                l = l >> 8;
            } else {
                break;
            }
        }
    }
    
    public static long [] createSubkeys (long key, byte [] rotations) {
        long subkeys [] = new long [16];
        key = permutePC1 (key);
        
        int c = (int) (key >> 28);
        int d = (int) (key & 0x0FFFFFFF);
        
        for (int i = 0; i < 16; i++) {
            if (rotations [i] == 1) {
                c = ( (c << 1) & 0x0FFFFFFF) | (c >> 27);
                d = ( (d << 1) & 0x0FFFFFFF) | (d >> 27);
            } else {
                c = ( (c << 2) & 0x0FFFFFFF) | (c >> 26);
                d = ( (d << 2) & 0x0FFFFFFF) | (d >> 26);
            }
            
            long cd = (c & 0xFFFFFFFFL) << 28 | (d & 0xFFFFFFFFL);
            subkeys [i] = permutePC2 (cd);
        }
        
        return subkeys;
    }
    
    public static int feistel (int r, /* 48 bits */ long subkey) {
        long e = permuteE (r);
        long x = e ^ subkey;
        int dst = 0;
        for (int i = 0; i < 8; i++) {
            dst >>>= 4;
            int s = permuteS (8 - i, (byte) (x & 0x3F));
            dst |= s << 28;
            x >>= 6;
        }
        
        return permuteP (dst);
    }
    
}
