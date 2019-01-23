package ru.shemplo.ml9.variance;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.*;

import java.util.concurrent.atomic.AtomicReference;

public class RunConditionalVariance {
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            @SuppressWarnings ("unused")
            int k = Integer.parseInt (br.readLine ()),
                n = Integer.parseInt (br.readLine ());
            Map <Integer, List <Integer>> xsToYs = new HashMap <> ();
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                int x = Integer.parseInt (st.nextToken ()),
                    y = Integer.parseInt (st.nextToken ());
                xsToYs.putIfAbsent (x, new ArrayList <> ());
                xsToYs.get (x).add (y);
            }
            
            AtomicReference <Double> variance = new AtomicReference <> (0D);
            xsToYs.entrySet ().forEach (e -> {
                double mean = e.getValue ().stream ().mapToDouble (i -> i).sum () 
                            / e.getValue ().size ();
                double average = e.getValue ().stream ().mapToDouble (i -> i)
                               . map (y -> Math.pow (y - mean, 2)).sum ()
                               / n;
                variance.getAndAccumulate (average, (v, u) -> v + u);
            });
            
            System.out.println (variance.get ());
        }
    }
    
}
