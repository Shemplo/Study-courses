package ru.shemplo.ml.lab6;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.StringTokenizer;

import ru.shemplo.ml.lab6.RunBF.Dataset.Letter;

public class RunBF {
    
    private static int n, t;
    
    public static void main (String ... args) throws IOException {
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            n = Integer.parseInt (br.readLine ());
            Dataset train = new Dataset ();
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                int len = Integer.parseInt (st.nextToken ());
                String verdict = st.nextToken ();
                
                st = new StringTokenizer (br.readLine ());
                Letter letter = new Letter (len);
                letter.label = verdict;
                for (int j = 0; j < len; j++) {
                    letter.content [j] = st.nextToken ();
                }
                
                train.dataset.add (letter);
            }
            
            Classifier classifier = new Classifier ();
            classifier.train (train);
            
            t = Integer.parseInt (br.readLine ());
            for (int i = 0; i < t; i++) {
                int len = Integer.parseInt (br.readLine ());
                
                StringTokenizer st = new StringTokenizer (br.readLine ());
                Letter letter = new Letter (len);
                for (int j = 0; j < len; j++) {
                    letter.content [j] = st.nextToken ();
                }
                
                System.out.println (classifier.predict (letter));
            }
        }
    }
    
    public static class Dataset {
        
        public static class Letter {
            
            public final String [] content;
            public String label;
            
            public Letter (int size) {
                this.content = new String [size];
            }
            
        }
        
        public final List <Letter> dataset = new ArrayList <> ();
        
    }
    
    public static class Classifier {
        
        private static class Side {
            
            private static double eps = 0.01;
            
            private int letters = 0, words = 0;
            private final Map <String, Integer> 
                entries = new HashMap <> ();
            
            public void addLetter (Letter letter) {
                letters += 1; words += letter.content.length;
                Arrays.asList (letter.content).forEach (word -> {
                    entries.compute (word, (k, v) -> v == null ? 1 : v + 1);
                });
            }
            
            public double getLikehoodWith (Letter letter, int totalLetters, int uniqueWords) {
                double part = Math.log (((double) letters) / totalLetters);
                for (String word : letter.content) {
                    double value = Optional.ofNullable (entries.get (word)).orElse (0) + eps;
                    part += Math.log (value / (words + uniqueWords * eps));
                }
                return part;
            }
            
        }
        
        private final Side spam = new Side (), legit = new Side ();
        private Set <String> uniqueWords = new HashSet <> ();
        private int letters = 0;
        
        public void train (Dataset dataset) {
            for (Letter letter : dataset.dataset) {
                uniqueWords.addAll (Arrays.asList (letter.content));
                letters += 1;
                
                switch (letter.label) {
                    case "L": legit.addLetter (letter); break;
                    case "S": spam.addLetter  (letter); break;
                }
            }
        }
        
        public String predict (Letter letter) {
            int words = uniqueWords.size ();
            double likehoodL = legit.getLikehoodWith (letter, letters, words),
                   likehoodS = spam.getLikehoodWith (letter, letters, words);
            return likehoodS > likehoodL * 0.95 ? "S" : "L";
        }
        
    }
    
}
