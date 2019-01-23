package ru.shemplo.ml9.chi2;

import static java.lang.Double.*;
import static java.lang.Math.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.*;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class RunChiSquare {
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            StringTokenizer stf = new StringTokenizer (br.readLine ());
            int kx = Integer.parseInt (stf.nextToken ()),
                n  = Integer.parseInt (br.readLine ());
            Map <Pair, Integer> pairs = new TreeMap <> (
                (a, b) -> a.F == b.F ? compare (a.S, b.S) : compare (a.F, b.F)
            );
            Map <Integer, Double> probabilityXs = new HashMap <> (),
                                  probabilityYs = new HashMap <> ();
            double part = 1d / n;
            
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                int x = Integer.parseInt (st.nextToken ()),
                    y = Integer.parseInt (st.nextToken ());
                pairs.compute (new Pair (x, y), (k, v) -> v == null ? 1 : v + 1);
                probabilityXs.compute (x, (k, v) -> v == null ? part : v + part);
                probabilityYs.compute (y, (k, v) -> v == null ? part : v + part);
            }
            
            AtomicReference <Double> chi     = new AtomicReference <> (0D),
                                     totalPY = new AtomicReference <> (1D);
            AtomicInteger prev = new AtomicInteger (1);
            
            pairs.entrySet ().forEach (e -> {
                double probX = probabilityXs.get (e.getKey ().F),
                       probY = probabilityYs.get (e.getKey ().S);
                double entropy = probX * probY * n,
                       dChi    = pow (e.getValue () - entropy, 2) / entropy;
                chi.getAndAccumulate (dChi, (v, u) -> v + u);
                
                for (int i = prev.get (); i < e.getKey ().F; i++) {
                    Double tProbX = probabilityXs.get (i);
                    chi.getAndAccumulate ((tProbX == null ? 0d : tProbX) * totalPY.get () * n, 
                                          (v, u) -> v + u);
                    totalPY.set (1.0D);
                }
                
                totalPY.getAndAccumulate (probY, (v, u) -> v - u);
                prev.set (e.getKey ().F);
            });
            
            for (int i = prev.get (); i <= kx; i++) {
                Double tProbX = probabilityXs.get (i);
                chi.getAndAccumulate ((tProbX == null ? 0d : tProbX) * totalPY.get () * n, 
                                      (v, u) -> v + u);
                totalPY.set (1.0D);
            }
            
            System.out.println (chi.get ());
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
