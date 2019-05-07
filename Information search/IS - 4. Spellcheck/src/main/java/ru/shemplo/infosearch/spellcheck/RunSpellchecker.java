package ru.shemplo.infosearch.spellcheck;

import java.io.IOException;
import java.io.PrintWriter;

import java.util.*;

import java.util.concurrent.ConcurrentHashMap;

import ru.shemplo.infosearch.spellcheck.forest.PrefixForest;
import ru.shemplo.infosearch.spellcheck.io.CSVReader;

public class RunSpellchecker {
    
    private static final String directory = "itmo-spelling-correction-2019/";
    private static final String fileOUT = "fixed.submission.csv";
    private static final String fileIN2 = "public.freq.csv";
    private static final String fileIN = "words.freq.csv";
    
    public static void main (String ... args) throws IOException {
        ///*
        final CSVReader wordsReader = new CSVReader ();
        wordsReader.read (directory + fileIN, ",");
        System.out.println ("IN file read");
        
        final PrefixForest forest = new PrefixForest ();
        for (int i = 0; i < wordsReader.getRowsNumber (); i++) {
            int freq = Integer.parseInt (wordsReader.get ("Freq", i));
            String word = wordsReader.get ("Id", i);
            forest.addWord (word, freq);
        }
        System.out.println ("Forest built");
        
        forest.normalize ();
        System.out.println ("Foretst normalized");
        
        final CSVReader answersReader = new CSVReader ();
        answersReader.read (directory + fileIN2, ",");
        System.out.println ("IN2 file read");
        
        final Map <String, Double> replaces = new ConcurrentHashMap <> ();
        final Set <String> toCorrect = new HashSet <> ();
        
        for (int i = 0; i < answersReader.getRowsNumber (); i++) {
            String expected = answersReader.get ("Expected", i);
            String word = answersReader.get ("Id", i);
            if (word.equals (expected)) {
                forest.addCorrectWord (word);
            } else {
                final int length = Math.min (word.length (), expected.length ());
                toCorrect.add (word);
                
                for (int j = 0; j < length; j++) {
                    if (word.charAt (j) != expected.charAt (j)) {
                        String key = word.charAt (j) + "-" + expected.charAt (j);
                        replaces.compute (key, (__, v) -> v == null ? 1 : v + 1);
                    }
                }
            }
        }
        System.out.println ("Correct words added");
        
        replaces.forEach ((key, value) -> {
            if (value < 1) { return; }
            
            String opposite = reverse (key);
            double opvalue = Optional.ofNullable (replaces.get (opposite))
                           . orElse (0.01D);
            double factor = value + opvalue;
            
            replaces.put (opposite, opvalue / factor);
            replaces.put (key, value / factor);
        });
        forest.setStatistics (replaces);
        System.out.println ("Statistics calculated");
        
        /*
        Iterator <String> iter = toCorrect.iterator ();
        iter.next ();
        
        String wordt = iter.next ();
        System.out.println (wordt + " " + forest.spellcheckWord (wordt));
        */
        
        final PrintWriter pw = new PrintWriter (directory + fileOUT);
        pw.println ("Id,Expected");
        
        System.out.println ("To fix: " + toCorrect.size () + " words");
        for (int i = 0; i < wordsReader.getRowsNumber (); i++) {
            final String word = wordsReader.get ("Id", i);
            String fixed = toCorrect.contains (word) 
                         ? forest.spellcheckWord (word)
                         : word;
            
            pw.println (word + "," + fixed);
            
            if (i % 1000 == 0) {                
                System.out.println (i + ": " + word + " -> " + fixed);
            }
        }
        pw.close ();
        System.out.println ("Spellcheck correction finished");
        
        //final CSVWriter wordsWriter = new CSVWriter (directory + fileOUT, ",");
        //wordsWriter.write (Arrays.asList ("Id", "Expected"), rows);
        //*/
        
        /*
        final PrefixForest forest = new PrefixForest ();
        forest.addWord ("", 2);
        forest.addWord ("man", 3);
        forest.addWord ("mat", 1);
        forest.addWord ("mit", 2);
        forest.addWord ("bat", 1);
        forest.addWord ("rat", 1);
        forest.addWord ("rating", 4);
        forest.addWord ("rum", 3);
        forest.addWord ("rumba", 3);
        forest.addWord ("mana", 2);
        forest.addWord ("manual", 2);
        forest.addWord ("mordor", 3);
        
        forest.normalize ();
        
        System.out.println (forest.spellcheckWord ("rut"));
        */
    }
    
    private static String reverse (String s) {
        char [] array = s.toCharArray ();
        int length = s.length ();
        for (int i = 0; i < length / 2; i++) {
            char tmp = array [i];
            array [i] = array [length - i - 1];
            array [length - i - 1] = tmp;
        }
        return new String (array);
    }
    
}
