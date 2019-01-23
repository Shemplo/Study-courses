package ru.shemplo.ml9.spearman;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.*;

public class RunSpearmanRank {
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            
            int n = Integer.parseInt (br.readLine ());
            List <Pair> xs = new ArrayList <> (),
                        ys = new ArrayList <> ();
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                int x = Integer.parseInt (st.nextToken ()),
                    y = Integer.parseInt (st.nextToken ());
                xs.add (new Pair (x, i)); ys.add (new Pair (y, i));
            }
            
            Comparator <Pair> comparator 
                = (a, b) -> a.F == b.F
                          ? Integer.compare (a.S, b.S)
                          : Integer.compare (a.F, b.F);
            xs.sort (comparator); ys.sort (comparator);
            
            int [] xR = new int [n], yR = new int [n];
            for (int i = 0; i < n; i++) {
                xR [xs.get (i).S] = yR [ys.get (i).S] = i;
            }
            
            double distance = 0.0d;
            for (int i = 0; i < n; i++) {
                distance += Math.pow (xR [i] - yR [i], 2) / n;
            }
            
            System.out.println (1.0 - (6.0 * distance) / (n * n - 1.0));
        }
    }
    
    public static class Pair {
        
        public final int F, S;
        
        public Pair (int f, int s) {
            this.F = f; this.S = s;
        }
        
    }
    
}
