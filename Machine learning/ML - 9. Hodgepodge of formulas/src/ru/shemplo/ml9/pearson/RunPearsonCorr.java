package ru.shemplo.ml9.pearson;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.*;

public class RunPearsonCorr {
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            int n = Integer.parseInt (br.readLine ());
            List <Pair> pairs = new ArrayList <> ();
            double avgX = 0, avgY = 0;
            
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                double x = Integer.parseInt (st.nextToken ()),
                       y = Integer.parseInt (st.nextToken ());
                avgX += x / n; avgY += y / n;
                pairs.add (new Pair (x, y));
            }
            
            double total = 0, sqTotalX = 0, sqTotalY = 0;
            for (int i = 0; i < n; i++) {
                double x = pairs.get (i).F - avgX, 
                       y = pairs.get (i).S - avgY;
                sqTotalX += Math.pow (x, 2);
                sqTotalY += Math.pow (y, 2);
                total += x * y; 
            }
            
            double denom = Math.sqrt (sqTotalX * sqTotalY);
            System.out.println (total / denom);
        }
    }
    
    public static class Pair {
        
        public final double F, S;
        
        public Pair (double f, double s) {
            this.F = f; this.S = s;
        }
        
    }
    
}
