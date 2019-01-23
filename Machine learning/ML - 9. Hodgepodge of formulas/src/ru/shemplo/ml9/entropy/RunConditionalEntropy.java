package ru.shemplo.ml9.entropy;

import static java.lang.Double.*;
import static java.lang.Math.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.*;

import java.util.concurrent.atomic.AtomicReference;

public class RunConditionalEntropy {
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            br.readLine (); // just skip
            
            int n = Integer.parseInt (br.readLine ());
            Map <Pair, Double> pairs = new TreeMap <> ((a, b) -> a.F == b.F
                                                               ? compare (a.S, b.S)
                                                               : compare (a.F, b.F));
            Map <Integer, Double> probabilities = new HashMap <> ();
            double part = 1d / n;
            
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                int x = Integer.parseInt (st.nextToken ()),
                    y = Integer.parseInt (st.nextToken ());
                pairs.compute (new Pair (x, y), (k, v) -> v == null ? part : v + part);
                probabilities.compute (x, (k, v) -> v == null ? part : v + part);
            }
            
            AtomicReference <Double> entropy = new AtomicReference <> (0D);
            pairs.entrySet ().forEach (e -> {
                double probX  = probabilities.get (e.getKey ().F),
                       probYX = e.getValue () / probX;
                entropy.getAndAccumulate (probX * (-probYX * log (probYX)), 
                                          (v, u) -> v + u);
            });
            
            System.out.println (entropy.get ());
        }
    }
    
    public static class Pair {
        
        public final int F, S;
        
        public Pair (int f, int s) {
            this.F = f; this.S = s;
        }
        
        @Override
        public boolean equals (Object obj) {
            if (this == obj) { return true; }
            
            if (obj instanceof Pair) {
                Pair other = (Pair) obj;
                return this.F == other.F
                    && this.S == other.S;
            }
            
            return false;
        }
        
        @Override
        public int hashCode () {
            return (int) (F * 1000000 + S);
        }
        
    }
    
}
