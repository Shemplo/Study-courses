package ru.shemplo.ml.lab3;

import static java.lang.Math.*;

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
        }),
        
        MANHATTAN ((a, b) -> {
            double dist = 0;
            for (int i = 0; i < Math.min (a.length, b.length); i++) { 
                dist += abs (a [i] - b [i]); 
            }
            return dist;
        }),
        
        CHEBYSHEV ((a, b) -> {
            double dist = 0;
            for (int i = 0; i < Math.min (a.length, b.length); i++) {
                dist = Math.max (dist, abs (a [i] - b [i]));
            }
            return dist;
        });
        
        private final BiFunction <double [], double [], Double> FUNCTION;
        
        private Metrics (BiFunction <double [], double [], Double> distance) {
            this.FUNCTION = distance;
        }
        
        public double distance (double [] a, double [] b) {
            return FUNCTION.apply (a, b);
        }
        
    }
    
    private static enum Kernel {
        
        UNIFORM  (d -> abs (d) <= 1 ? 1.0d : 0.0d),
        TRIANGLE (d -> abs (d) <= 1 ? 1 - abs (d) : 0.0d),
        EXPONENT (d -> abs (d) > 0 ? 1 / abs (d) : 1.0d),
        TRICUBE  (d -> abs (d) <= 1 ? 0.86419753 * pow (1 - pow (d, 3), 3) : 0.0d);
        
        private final Function <Double, Double> FUNCTION;
        
        private Kernel (Function <Double, Double> weight) {
            this.FUNCTION = weight;
        }
        
        public double scale (double distance) {
            return FUNCTION.apply (distance);
        }
        
    }
    
    private static List <double []> TRAIN = new ArrayList <> (),
                                    CLEAR = new ArrayList <> (),
                                    TEST  = new ArrayList <> ();

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
        CLEAR.addAll (TRAIN); // backup
        
        //normalize (TRAIN, null, features);
        Collections.shuffle (TRAIN); 
        train (classes);
        
        int testSize = Integer.parseInt (br.readLine ());
        for (int i = 0; i < testSize; i++) {
            StringTokenizer st = new StringTokenizer (br.readLine ());
            double [] entry = new double [features];
            
            for (int j = 0; j < features; j++) {
                entry [j] = Double.parseDouble (st.nextToken ());
            }
            
            TEST.add (entry);
        }
        
        TRAIN.clear (); TRAIN.addAll (CLEAR);
        //normalize (TEST, TRAIN, features);
        
        for (int i = 0; i < testSize; i++) {
            List <Pair <Double, Integer>> weights = getWeights (TEST.get (i), -1, 
                                    bestK, bestWindow, bestMetrics, bestKernel);
            StringBuilder sb = new StringBuilder ();
            sb.append (bestK).append (" ");
            for (int j = 0; j < bestK; j++) {
                Pair <Double, Integer> pair = weights.get (j);
                sb.append (pair.getValue () + 1).append (" ")
                  .append (pair.getKey ()).append (" ");
            }
            System.out.println (sb.toString ());
        }
    }
    
    @SuppressWarnings ("unused")
    private static void normalize (List <double []> data, 
            List <double []> support, int features) {
        double [][] bounds = new double [features][3];
        for (int i = 0; i < features; i++) {
            bounds [i][0] = Double.MIN_VALUE;
            bounds [i][1] = Double.MAX_VALUE;
        }
        
        for (int i = 0; i < data.size (); i++) {
            double [] entry = data.get (i);
            for (int j = 0; j < features; j++) {
                bounds [j][0] = Math.max (bounds [j][0], entry [j]);
                bounds [j][1] = Math.min (bounds [j][1], entry [j]);
                bounds [j][2] += entry [j];
            }
        }
        
        int total = data.size ();
        
        if (support != null) {
            for (int i = 0; i < support.size (); i++) {
                double [] entry = support.get (i);
                for (int j = 0; j < features; j++) {
                    bounds [j][0] = Math.max (bounds [j][0], entry [j]);
                    bounds [j][1] = Math.min (bounds [j][1], entry [j]);
                    bounds [j][2] += entry [j];
                }
            }
            
            total += support.size ();
        }
        
        for (int i = 0; i < data.size (); i++) {
            for (int j = 0; j < features; j++) {
                double value = data.get (i) [j]; 
                value = (value - bounds [j][2] / total) 
                      / (bounds [j][0] - bounds [j][1]);
                data.get (i) [j] = value;
            }
        }
        
        if (support != null) {
            for (int i = 0; i < support.size (); i++) {
                for (int j = 0; j < features; j++) {
                    double value = support.get (i) [j]; 
                    value = (value - bounds [j][2] / total) 
                          / (bounds [j][0] - bounds [j][1]);
                    support.get (i) [j] = value;
                }
            }
        }
    }
    
    private static Metrics bestMetrics; 
    private static Kernel  bestKernel;
    private static double  bestWindow;
    private static int     bestK;
    
    private static void train (int classes) {
        int neighboursNumber = 20, windowsNumber = 15,
            metricsNumber = Metrics.values ().length,
            kernelNumber = Kernel.values ().length;
        
        double bestScore = 0;
        for (int i = 1; i <= neighboursNumber; i++) {
            for (int j = 0; j < metricsNumber; j++) {
                Metrics metrics = Metrics.values () [j];
                for (int k = 0; k < kernelNumber; k++) {
                    Kernel kernel = Kernel.values () [k];
                    for (int w = 1; w <= windowsNumber; w++) {
                        double window = 0.1 + (5 - 0.1) / windowsNumber * w;
                        double score = runLeaveOneOut (i, window, 
                                  metrics, kernel, 100, classes);
                        if (score > bestScore) {
                            bestMetrics = metrics;
                            bestKernel = kernel;
                            bestWindow = window;
                            bestK = i;
                            
                            bestScore = score;
                        }
                    }
                }
            }
        }
        
        /*
        System.out.println ("Best: " + bestScore + " " + bestK + " " + bestWindow 
                         + " " + bestMetrics + " " + bestKernel);*/
    }
    
    private static List <Pair <Double, Integer>> getWeights (double [] point, 
            int ignore, int k, double window, Metrics metrics, Kernel kernel) {
        List <Pair <Double, Integer>> weights = new ArrayList <> ();
        for (int j = 0; j < TRAIN.size (); j++) {
            if (ignore == j) { continue; } // skipping test point
            double distance = metrics.distance (point, TRAIN.get (j));
            double weight  = kernel.scale (distance / window);
            weights.add (new Pair <> (weight, j));
        }
        
        weights.sort ((a, b) -> -Double.compare (a.getKey (), b.getKey ())); // Descending
        return weights;
    }
    
    private static Map <Integer, Double> runClassification (double [] point, 
            int ignore, int k, double window, Metrics metrics, Kernel kernel) {
        List <Pair <Double, Integer>> weights = getWeights (point, ignore, k, window, metrics, kernel);
        Map <Integer, Double> sums = new HashMap <> ();
        
        for (int j = 0; j < Math.min (k, weights.size ()); j++) {
            Pair <Double, Integer> pair = weights.get (j);
            double [] train = TRAIN.get (pair.getValue ());
            int cluster = (int) train [train.length - 1];
            sums.putIfAbsent (cluster, 0d);
            
            sums.compute (cluster, 
                (ind, v) -> v + pair.getKey ());
        }
        
        return sums;
    }
    
    private static double runLeaveOneOut (int k, double window, 
            Metrics metrics, Kernel kernel, int iterations, int classes) {
        double [][] matrix = new double [classes][classes];
        iterations = Math.min (iterations, TRAIN.size ());
        
        for (int i = 0; i < iterations; i++) {
            double [] tmp = TRAIN.get (i), test = Arrays.copyOf (tmp, tmp.length - 1);
            Map <Integer, Double> 
                sums = runClassification (test, i, k, window, metrics, kernel);
            
            int maxIndex = -1; double maxRate = -1;
            for (Integer key : sums.keySet ()) {
                if (maxRate < sums.get (key)) {
                    maxRate = sums.get (key);
                    maxIndex = key - 1;
                }
            }
            
            //System.out.println (sums + " " + maxIndex + " " + (int) (tmp [tmp.length - 1]));
            //if ((int) (tmp [tmp.length - 1]) == maxIndex) { tp++; }
            matrix [maxIndex][(int) tmp [tmp.length - 1] - 1] += 1;
        }
        
        return f1 (matrix);
    }
    
    private static double f1 (double [][] matrix) {
        double [] columns = new double [matrix.length],
                  lines   = new double [matrix.length];
        double total = 0, score = 0;
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix [i].length; j++) {
                columns [j] += matrix [i][j];
                lines [i] += matrix [i][j];
                total += matrix [i][j];
            }
        }
        
        for (int i = 0; i < matrix.length; i++) {
            double tp = matrix [i][i];
            double precision = (tp == 0) ? 0 : (tp / columns [i]),
                   recall    = (tp == 0) ? 0 : (tp / lines [i]);
            if (Math.abs (precision + recall) >= 1e-4) {
                score += 2 * (precision * recall) 
                       / (precision + recall) * lines [i];
            }
        }
        
        return score / total;
    }
    
}
