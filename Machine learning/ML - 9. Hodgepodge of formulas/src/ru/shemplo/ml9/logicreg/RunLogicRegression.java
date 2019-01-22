package ru.shemplo.ml9.logicreg;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.Locale;
import java.util.StringJoiner;

public class RunLogicRegression {
    
    public static final int MAX_M = 10;
    
    public static int m, powerM;
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            m = Integer.parseInt (br.readLine ());
            powerM = 1 << m;
            
            int [] f = new int [powerM];
            for (int i = 0; i < f.length; i++) {
                // Converting 0 -> -1, 1 -> 1
                f [i] = Integer.parseInt (br.readLine ()) * 2 - 1;
            }
            System.out.println (String.format ("3 %d %d 1", m, f.length));
            
            boolean [] active = new boolean [MAX_M];
            int counter = 0;
            
            for (int i = 0; i < f.length; i++) {
                for (int j = MAX_M - 1; j >= MAX_M - m; j--) {
                    System.out.print (active [j] ? "1 " : "-1 ");
                }
                System.out.println (0.5 - counter);
                
                counter = count (counter, active);
            }
            
            StringJoiner sj = new StringJoiner (" ", "", " 0");
            for (int i = 0; i < f.length; i++) {
                sj.add ("" + f [i]);
            }
            
            System.out.println (sj.toString ());
        }
    }
    
    private static int count (int counter, boolean [] active) {
        for (int i = MAX_M - 1; i >= 0; i--) {
            active [i] = !active [i];
            
            if (active [i]) { // after swap
                counter += 1;
                break;
            }
            
            counter -= 1;
        }
        
        return counter;
    }
    
}
