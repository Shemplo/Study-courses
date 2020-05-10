package ru.shemplo.crypto.lab4;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Random;

public class Utils {
    
    public static String readInput () throws IOException {
        var r = new InputStreamReader (System.in);
        var br = new BufferedReader (r);
        
        return br.readLine ();
    }
    
    public static void randomizeArray (int [] array, Random random, int limit) {
        for (int i = 0; i < array.length; i++) {
            array [i] = random.nextInt (limit);
        }
    }
    
    public static int majority (int a, int b, int c) {
        return a + b + c >= 2 ? 1 : 0;
    }
    
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
