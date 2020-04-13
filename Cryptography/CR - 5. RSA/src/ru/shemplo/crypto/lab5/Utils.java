package ru.shemplo.crypto.lab5;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Utils {
    
    public static String readInput () throws IOException {
        var r  = new InputStreamReader (System.in);
        var br = new BufferedReader (r);
        
        return br.readLine ();
    }
    
    public static int gcd (int a, int b) {
        if (b > a) {
            final int t = b; 
            b = a; a = t;
        }
        
        while (b > 0 && a % b > 0) {
            final int t = a % b;
            a = b; b = t;
        }
        
        return b == 0 ? a : b;
    }
    
}
