package ru.shemplo.ml.lab3;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.function.BiFunction;
import java.util.function.Function;

import javafx.util.Pair;

public class RunKNN {
    
    private static enum Metrics {
        
        EUCLIDIAN ((a, b) -> {
            double sum = 0;
            for (int i = 0; i < Math.min (a.length, b.length); i++) { 
                sum += (a [i] - b [i]) * (a [i] - b [i]); 
            }
            return Math.sqrt (sum);
        }); 
        
        //MANHATTAN ((a, b) -> 0d);
        
        private final BiFunction <double [], double [], Double> FUNCTION;
        
        private Metrics (BiFunction <double [], double [], Double> distance) {
            this.FUNCTION = distance;
        }
        
        public double distance (double [] a, double [] b) {
            return FUNCTION.apply (a, b);
        }
        
    }
    
    private static enum Kernel {
        
        UNIFORM (d -> Math.abs (d) <= 1 ? d : 0);
        
        private final Function <Double, Double> FUNCTION;
        
        private Kernel (Function <Double, Double> weight) {
            this.FUNCTION = weight;
        }
        
        public double scale (double distance) {
            return FUNCTION.apply (distance);
        }
        
    }
    
    private static List <double []> TRAIN = new ArrayList <> ();

    public static void main (String ... args) throws IOException {
        try (
            Reader r = new InputStreamReader (System.in, StandardCharsets.UTF_8);
            BufferedReader br = new BufferedReader (r);
        ) { solve (br); }
    }
    
    private static void solve (BufferedReader br) throws IOException {
        int features  = Integer.parseInt (br.readLine ()),
            classes   = Integer.parseInt (br.readLine ()),
            trainSize = Integer.parseInt (br.readLine ());
        for (int i = 0; i < trainSize; i++) {
            StringTokenizer st = new StringTokenizer (br.readLine ());
            double [] entry = new double [features + 1];
            
            for (int j = 0; j < features; j++) {
                entry [j] = Double.parseDouble (st.nextToken ()); 
            }
            
            double answer = Double.parseDouble (st.nextToken ());
            entry [entry.length - 1] = answer;
            TRAIN.add (entry);
        }
        
        Collections.shuffle (TRAIN); train ();
        
        int testSize = Integer.parseInt (br.readLine ());
        for (int i = 0; i < testSize; i++) {
            StringTokenizer st = new StringTokenizer (br.readLine ());
            double [] entry = new double [features];
            
            for (int j = 0; j < features; j++) {
                double feature = Double.parseDouble (st.nextToken ());
            }
        }
    }
    
    private static Metrics bestMetrics; 
    private static Kernel bestKernel;
    private static int kBest;
    
    private static void train () {
        int neighboursNumber = 20,
            metricsNumber = Metrics.values ().length,
            kernelNumber = Kernel.values ().length;
        
        double bestScore = 0;
        for (int i = 1; i <= neighboursNumber; i++) {
            for (int j = 0; j < metricsNumber; j++) {
                Metrics metrics = Metrics.values () [j];
                for (int k = 0; k < kernelNumber; k++) {
                    Kernel kernel = Kernel.values () [k];
                    
                    double score = runLeaveOneOut (i, metrics, kernel, 10);
                    System.out.println (score);
                    if (score > bestScore) {
                        bestMetrics = metrics;
                        bestKernel = kernel;
                        kBest = i;
                        
                        bestScore = score;
                    }
                }
            }
        }
    }
    
    private static double runLeaveOneOut (int k, Metrics metrics, Kernel kernel, int iterations) {
        int tp = 0;
        for (int i = 0; i < Math.min (iterations, TRAIN.size ()); i++) {
            List <Pair <Double, Integer>> weights = new ArrayList <> ();
            
            double [] tmp = TRAIN.get (i),
                      test = Arrays.copyOf (tmp, tmp.length - 1);
            for (int j = 0; j < TRAIN.size (); j++) {
                double dist = kernel.scale (metrics.distance (test, TRAIN.get (j)));
                weights.add (new Pair <> (dist, (int) TRAIN.get (j) [tmp.length - 1]));                
            }
            
            // Distance from test point to itself == 0 -> it won't be in the head
            weights.sort ((a, b) -> Double.compare (a.getKey (), b.getKey ()));
            
            Map <Integer, Double> sums = new HashMap <> ();
            int maxIndex = -1; double maxRate = 0;
            for (int j = 0; j < Math.min (k, weights.size ()); j++) {
                Pair <Double, Integer> pair = weights.get (j);
                sums.putIfAbsent (pair.getValue (), 0d);
                
                int id = pair.getValue ();
                sums.compute (id, (ind, v) -> v + pair.getKey ());
                if (sums.get (id) > maxRate) {
                    maxRate = sums.get (id);
                    maxIndex = j;
                }
            }
            
            System.out.println (sums);
            if ((int) (tmp [tmp.length - 1]) == maxIndex) { tp++; }
        }
        
        return (0.0 + tp) / iterations;
    }
    
}
