package ru.shemplo.ml.lab8;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

public class RunDNN {
    
    private static final double     startTime = System.currentTimeMillis ();
    private static final List <Obj> dataset   = new ArrayList <> ();
    private static final Random     R         = new Random (239);
    
    private static final List <Double> averages  = new ArrayList <> (),
                                       standatrs = new ArrayList <> ();
    
    private static int [] layersSize;
    private static int d, t, m;
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            d = Integer.parseInt (br.readLine ());
            layersSize = new int [d - 1];
            
            StringTokenizer st = new StringTokenizer (br.readLine ());
            m = Integer.parseInt (st.nextToken ());
            for (int i = 0; i < layersSize.length; i++) {
                layersSize [i] = Integer.parseInt (st.nextToken ());
            }
            
            Collections.shuffle (dataset);
            
            t = Integer.parseInt (br.readLine ());
            for (int i = 0; i < t; i++) {
                st = new StringTokenizer (br.readLine ());
                Obj obj = new Obj (m);
                for (int j = 0; j < m; j++) {
                    final String value = st.nextToken ();
                    obj.features [j] = Double.parseDouble (value);
                }
                obj.classID = Integer.parseInt (st.nextToken ());
                dataset.add (obj);
            }
            
            makeNormalization ();
            initializeNetwork ();
            train             ();
            
            network.printAnswer ();
        }
    }
    
    // INPUT PART //
    
    private static class Obj {
        
        public final double [] features;
        public int classID;
        
        public Obj (int size) {
            this.features = new double [size];
        }
        
    }
    
    private static void makeNormalization () {
        double [] inDataset = new double [t];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < t; j++) {
                inDataset [j] = dataset.get (j).features [i];
            }
            
            double avg = getAverage  (inDataset), 
                   std = getStandart (inDataset);
            if (std == 0.0) { std = 1.0; } 
            standatrs.add (std);
            averages.add (avg);
            
            for (int j = 0; j < t; j++) {
                double value = dataset.get (j).features [i];
                dataset.get (j).features [i] = (value - avg) / std;
            }
        }
    }
    
    private static double getAverage (double [] array) {
        return DoubleStream.of (array).average ().orElse (0);
    }
    
    private static double getStandart (double [] array) {
        double average = getAverage (array);
        return Math.sqrt (
            DoubleStream.of (array)
            . map (v -> Math.pow (v - average, 2))
            . sum () 
            / array.length
        );
    }
    
    // NEWTWORK PART //
    
    private static final Network network = new Network ();
    
    private static void initializeNetwork () {
        int inputSize = m;
        
        for (int i = 0; i < layersSize.length; i++) {
            List <Neuron> layer = new ArrayList <> ();
            network.layers.add (layer);
            
            for (int j = 0; j < layersSize [i]; j++) {
                layer.add (new Neuron (inputSize));
            }
            
            inputSize = layersSize [i];
        }
    }
    
    private static class Network {
        
        private final List <List <Neuron>> layers 
              = new ArrayList <> ();
        
        public void trainForward (Obj input) {
            double [] data = input.features;
            
            for (List <Neuron> layer : layers) {
                double [] output = new double [layer.size ()];
                for (int i = 0; i < layer.size (); i++) {
                    final Neuron neuron = layer.get (i);
                      neuron.activateBy (data);
                    output [i] = neuron.output;
                }
                
                data = output;
            }
        }
        
        public void trainBackward (int correct) {
            Neuron last = layers.get (layers.size () - 1).get (0);
            last.delta = (correct - last.output) 
                       * dOfTanh (last.output);
            
            for (int i = layers.size () - 1 - 1; i >= 0; i--) {
                final List <Neuron> layer = layers.get (i);
                for (int j = 0; j < layer.size (); j++) {
                    Neuron neuron = layer.get  (j);
                    final int fj = j;
                    
                    neuron.delta = layers.get  (i + 1).stream ()
                                 . mapToDouble (n -> n.delta * n.weights [fj])
                                 . sum         () * dOfTanh (neuron.output);
                }
            }
        }
        
        private static double LR = 0.01;
        
        public void correctWeights (Obj input) {
            for (int i = 0; i < layers.size (); i++) {
                List <Double> tmp = new ArrayList <> ();
                if (i == 0) {
                    for (int j = 0; j < input.features.length; j++) {
                        tmp.add (input.features [j]);
                    }
                } else {
                    tmp = layers.get (i - 1).stream ()
                        . map     (n -> n.output)
                        . collect (Collectors.toList ());
                }
                
                final Double [] data = tmp.toArray (new Double [0]);
                layers.get (i).forEach (n -> {
                    for (int j = 0; j < data.length; j++) {
                        n.weights [j] += LR * n.delta * data [j];
                    }
                    n.bias += LR * n.delta;
                });
            }
        }
        
        private double dOfTanh (double point) {
            return 1 - Math.pow (point, 2);
        }
        
        public void printAnswer () {
            List <Neuron> first = layers.get (0);
            first.forEach (n -> {
                double shift = 0;
                for (int i = 0; i < n.weights.length; i++) {
                    shift += (averages.get (i) * n.weights [i])
                           / standatrs.get (i);
                    double value = n.weights [i] / standatrs.get (i);
                    System.out.print (String.format ("%f ", value));
                }
                
                System.out.println (n.bias - shift);
            });
            
            for (int i = 1; i < layers.size (); i++) {
                layers.get (i).forEach (n -> {
                    StringJoiner sj = new StringJoiner (" ");
                    for (int j = 0; j < n.weights.length; j++) {
                        sj.add ("" + n.weights [j]);
                    }
                    
                    System.out.println (String.format ("%s %f", 
                                      sj.toString (), n.bias));
                });
            }
        }
        
    }
    
    private static class Neuron {
        
        public double output = 0.0, delta = 0.0;
        public double bias = R.nextDouble ();
        public final double [] weights;
        
        public Neuron (int size) {
            this.weights = new double [size];
            for (int i = 0; i < size; i++) {
                weights [i] = R.nextDouble ();
            }
        }
        
        public void activateBy (final double [] input) {
            output = Stream.iterate (0, i -> i + 1)
                   . limit       (weights.length)
                   . mapToDouble (i -> weights [i] * input [i])
                   . sum         () + bias;
            output = Math.tanh (output);
            output = Double.isFinite (output)
                   ? output : 0.0;
        }
        
    }
    
    public static void train () {
        double time = System.currentTimeMillis ();
        while (time - startTime < 8900) {
            for (Obj obj : dataset) {
                network.trainForward   (obj);
                network.trainBackward  (obj.classID);
                network.correctWeights (obj);
            }
            
            time = System.currentTimeMillis ();
        }
    }
    
}
