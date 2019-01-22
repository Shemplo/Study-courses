package ru.shemplo.ml9.distance;

import static java.lang.Integer.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.*;

public class RunDistances {
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            @SuppressWarnings ("unused")
            int k = Integer.parseInt (br.readLine ()),
                n = Integer.parseInt (br.readLine ());
            List <Pair> pairs = new ArrayList <> ();
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                pairs.add (new Pair (parseInt (st.nextToken ()), 
                                     parseInt (st.nextToken ())));
            }
            
            Comparator <Pair> comparator 
                = (a, b) -> a.F == b.F
                          ? Double.compare (a.S, b.S)
                          : Double.compare (a.F, b.F);
            pairs.sort (comparator);
            
            long similarity = 0, difference = 0;
            for (int i = 1; i < n; i++) {
                similarity += (pairs.get (i).F - pairs.get (i - 1).F) * i;
                difference += similarity;
            }
            difference *= 2;
            
            comparator = (a, b) -> a.S == b.S
                       ? Double.compare (a.F, b.F)
                       : Double.compare (a.S, b.S);
            pairs.sort (comparator);
            
            long same = 1, distance = 0;
            similarity = 0;
            
            for (int i = 1; i < n; i++) {
                Pair prev = pairs.get (i - 1),
                     cur  = pairs.get (i);
                if (prev.S == cur.S) {
                    similarity += (cur.F - prev.F) * same;
                    distance += similarity;
                    same += 1;
                } else {
                    similarity = 0;
                    same = 1;
                }
            }
            distance *= 2;
            
            System.out.println (distance);
            System.out.println (difference - distance);
        }
    }
    
    public static class Pair {
        
        public final double F, S;
        
        public Pair (double f, double s) {
            this.F = f; this.S = s;
        }
        
    }
    
}
