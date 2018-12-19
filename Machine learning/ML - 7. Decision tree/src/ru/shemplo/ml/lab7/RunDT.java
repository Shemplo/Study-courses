package ru.shemplo.ml.lab7;

import static java.lang.Double.*;
import static java.util.stream.Collectors.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.function.BiFunction;
import java.util.function.Function;

public class RunDT {
    
    private static final List <Obj> dataset = new ArrayList <> ();
    private static final int MAX_DEPTH = 11;
    @SuppressWarnings ("unused")
    private static int m = 0, k = 0, n = 0;
    
    public static void main (String ... args) throws IOException {
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            StringTokenizer st = new StringTokenizer (br.readLine ());
            m = Integer.parseInt (st.nextToken ());
            k = Integer.parseInt (st.nextToken ());
            n = Integer.parseInt (br.readLine ());
            
            for (int i = 0; i < n; i ++) {
                st = new StringTokenizer (br.readLine ());
                Obj obj = new Obj (i);
                
                for (int j = 0; j < m; j++) {
                    final String token = st.nextToken ();
                    obj.features.put (j, parseDouble (token));
                }
                obj.classNumber = Integer.parseInt (st.nextToken ());
                dataset.add (obj);
            }
            
            Tree tree = new Tree ();
            
            System.out.println (tree.nodes);
            tree.printByDFS ();
        }
    }
    
    private static class Obj {
        
        private int orderNumber, classNumber;
        private final Map <Integer, Double> 
            features = new HashMap <> ();
        
        public Obj (int order) { this.orderNumber = order; }
        
        @SuppressWarnings ("unused")
        public int getClassNumber () { return classNumber; }
        
    }
    
    private static class Tree {
        
        private int depth = 1, nodes = 0; // initial depth
        private final Node root;
        
        public Tree () { this.root = new Node (RunDT.dataset).build (this, depth); }
        
        private static class Node {
            
            private static BiFunction <Integer, Integer, Integer> 
                mapper = (k, v) -> v == null ? 1 : v + 1;
            private static BiFunction <Integer, Double, Function <Obj, Boolean>> 
                splitter = (feature, value) -> obj -> obj.features.get (feature) < value;
                
            private final List <Obj> dataset;
            private double score = 0d;// gini
            private Node left, right;
            
            private boolean isLeaf = false; // Only for splitting
            private int feature, id, classNumber;
            private double value; 
            
            @SuppressWarnings ("unused")
            public Node (int id) {
                this.dataset = null;
                this.id = id;
            }
            
            public Node (List <Obj> dataset) {
                this.dataset = dataset;
                
                final Map <Integer, Integer> entries = new HashMap <> ();
                this.dataset.forEach (obj -> entries.compute (obj.classNumber, mapper));
                
                this.score = 1 - entries.values ().stream ()
                           . mapToDouble (v -> (double) v)
                           . map         (v -> v / entries.size ())
                           . map         (v -> v * v)
                           . sum         ();
            }
            
            private static enum FinishCondition {
                MAX_DEPTH, DROUGHT, EQUIVALENCE,
                NOT_FINISH
            }
            
            public void split (int feature, double value) {
                this.feature = feature; this.value = value;
                
                Map <Boolean, List <Obj>> split = this.dataset.stream ()
                . collect (groupingBy (splitter.apply (feature, value)));
                
                this.right = new Node (split.get (false));
                this.left = new Node (split.get (true));
            }
            
            public Node build (Tree tree, int depth) {
                this.id = tree.nextID ();
                switch (checkConditions (depth)) {
                    case MAX_DEPTH: { // select class by the maximum of entries
                        final Map <Integer, Integer> entries = new HashMap <> ();
                        dataset.forEach (obj -> entries.compute (obj.classNumber, mapper));
                        
                        final BiFunction <Integer, Integer, Integer> max 
                            =  (a, b) -> entries.get (a) < entries.get (b) ? b : a;
                            
                        this.classNumber = entries.keySet ().stream ()
                        . reduce   (null, (cur, key) -> cur == null ? key 
                                                      : max.apply (cur, key))
                        . intValue ();
                        
                        this.isLeaf = true;
                        return this;
                    }
                    
                    case DROUGHT: {
                        this.classNumber = 1;
                        this.isLeaf = true;
                        return this;
                    }
                    
                    case EQUIVALENCE: {
                        this.classNumber = dataset.get (0)
                                         . classNumber;
                        this.isLeaf = true;
                        return this;
                    }
                    
                    case NOT_FINISH: break; // go to next splitting
                }
                
                double [] split = findSplitFeature ();
                split ((int) split [0], split [1]);
                this.left.build (tree, depth + 1);
                this.right.build (tree, depth + 1);
                
                return this;
            }
            
            private FinishCondition checkConditions (int depth) {
                if (depth == MAX_DEPTH) {
                    return FinishCondition.MAX_DEPTH; 
                }
                
                if (dataset == null || dataset.isEmpty ()) {
                    return FinishCondition.DROUGHT;
                }
                
                int classNumber = dataset.get (0).classNumber;
                long same = dataset.stream ()
                          . filter (obj -> obj.classNumber == classNumber)
                          . count  ();
                if (same == dataset.size ()) { return FinishCondition.EQUIVALENCE; }
                
                return FinishCondition.NOT_FINISH;
            }
            
            private double [] findSplitFeature () {
                final double [] result = new double [2];
                Arrays.fill (result, Integer.MIN_VALUE);
                double memory = Integer.MIN_VALUE;
                
                Set <Integer> features = this.dataset.get (0)
                                       . features.keySet ();
                for (Integer feature : features) {
                    List <Double []> list = new ArrayList <> ();
                    for (Obj obj : this.dataset) {
                        list.add (new Double [] {
                            (double) obj.orderNumber, (double) feature, 
                            obj.features.get (feature), (double) obj.classNumber
                        });
                    }
                    Collections.sort (list, (a, b) -> Double.compare (a [2], b [2]));
                    GiniScorer left = new GiniScorer (), right = new GiniScorer (list);
                    
                    for (int i = 0; i < list.size () - 1; i++) {
                        Double [] current = list.get (i), next = list.get (i + 1);
                        double fValue = ((double) (next [2] + current [2])) / 2;
                        left.applyChanges (current [3].intValue ());
                        right.applyChanges (next [3].intValue ());
                        
                        double uScore = score
                                      - ((double) left.size) / dataset.size () * left.getScore ()
                                      - ((double) right.size) / dataset.size () * right.getScore ();
                        if (uScore > memory) {
                            memory = uScore; result [0] = feature; result [1] = fValue;
                        }
                    }
                }
                
                return result;
            }
            
            public void printByDFS () {
                if (isLeaf) {
                    System.out.println (String.format ("C %d", classNumber));
                } else {
                    System.out.println (String.format ("Q %d %f %d %d", 
                               feature + 1, value, left.id, right.id));
                    left.printByDFS (); right.printByDFS ();
                }
            }
            
        }
        
        public void printByDFS () {
            root.printByDFS ();
        }
        
        private int nextID () { return ++nodes; }
        
    }
    
    private static class GiniScorer {
        
        private final Map <Integer, Integer> 
            groupBy = new HashMap <> ();
        private final boolean isFull;
        private int size, score;
        
        public GiniScorer () { this (new ArrayList <> ()); }
        
        public GiniScorer (List <Double []> dataset) {
            this.isFull = dataset.size () > 0;
            this.size = dataset.size ();
            
            dataset.forEach (obj -> groupBy.compute (obj [3].intValue (), 
                                       (k, v) -> v == null ? 0 : v + 1));
            this.score = 0;
            groupBy.values ().forEach (v -> score += v * v);
        }
        
        private final Function <Integer, Integer> S = v -> v * v;
        
        public void applyChanges (int classNumber) {
            score -= S.apply (groupBy.getOrDefault (classNumber, 0));
            
            if (isFull) {
                groupBy.computeIfPresent (classNumber, (k, v) -> v - 1);
                if (groupBy.getOrDefault (classNumber, 0) == 0) {
                    groupBy.remove (classNumber);
                }
            } else {
                groupBy.compute (classNumber, (k, v) -> v == null ? 0 : v + 1);
            }
            
            score += S.apply (groupBy.getOrDefault (classNumber, 0));
            size += isFull ? -1 : 1;
        }
        
        public double getScore () {
            if (size == 0) { return 1.0d; }
            return 1.0 - ((double) score) 
                         / S.apply (size);
        }
        
    }
    
}
