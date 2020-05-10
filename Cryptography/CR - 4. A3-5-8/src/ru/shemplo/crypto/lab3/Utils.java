package ru.shemplo.crypto.lab3;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Utils {
    
    public static String readInput () throws IOException {
        var r = new InputStreamReader (System.in);
        var br = new BufferedReader (r);
        
        return br.readLine ();
    }
    
    
    /*
     * 
     * 
for (i=0; i<16; i++)
    key[i] = (hextoint(argv[1][2*i+2])<<4)
            | hextoint(argv[1][2*i+3]);
     * 
     */
    
    public static int [] compressInput (String input) {
        int [] bytes = new int [16];
        
        for (int i = 0; i < 16; i++) {
            int a = Integer.parseInt (String.valueOf (input.charAt (2 * i)), 16);
            int b = Integer.parseInt (String.valueOf (input.charAt (2 * i + 1)), 16);
            bytes [i] = ((a << 4) | b) & 0xFF; 
        }
        
        return bytes;
    }
    
}
