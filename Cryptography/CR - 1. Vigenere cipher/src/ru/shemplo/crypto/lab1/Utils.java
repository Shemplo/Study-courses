package ru.shemplo.crypto.lab1;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Utils {
    
    public static String readInput () throws IOException {
        var r  = new InputStreamReader (System.in);
        var br = new BufferedReader (r);
        
        return br.readLine ();
    }
    
}
