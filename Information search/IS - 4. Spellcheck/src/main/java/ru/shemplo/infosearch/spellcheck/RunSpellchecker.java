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
        final CSVReader wordsReader = new CSVReader ();
        wordsReader.read (directory + fileIN, ",");
        System.out.println ("IN file read");
        
        @SuppressWarnings ("unused")
        Map <String, List <String>> metaphone = new HashMap <> ();
        final PrefixForest forest = new PrefixForest ();
        
        for (int i = 0; i < wordsReader.getRowsNumber (); i++) {
            int freq = Integer.parseInt (wordsReader.get ("Freq", i));
            String word = wordsReader.get ("Id", i);
            
            if (word.equals ("ЛЬВОВ") || word.equals ("ДЬЯВОЛ")) {
                System.out.println (word + " / " + freq);
            }
            
            forest.addWord (word, freq);
        }
        System.out.println ("Forest built");
        
        forest.normalize ();
        System.out.println ("Foretst normalized");
        
        final CSVReader answersReader = new CSVReader ();
        answersReader.read (directory + fileIN2, ",");
        System.out.println ("IN2 file read");
        
        final Map <String, Double> replaces = new ConcurrentHashMap <> ();
        final Map <String, String> toCorrect = new HashMap <> ();
        final Set <String> correct = new HashSet <> ();
        int replacesCount = 0;
        
        for (int i = 0; i < answersReader.getRowsNumber (); i++) {
            String expected = answersReader.get ("Expected", i);
            String word = answersReader.get ("Id", i);
            if (word.equals (expected)) {
                correct.add (word);
            } else {
                /*
                 * Calculating statistics of replaces particular symbols to other
                 * 
                 * For example: 
                 * 
                 * Id:       aabc
                 * Expected: abbd
                 * 
                 * Here letter 'a' must be replaced with 'b' (and 'c' with 'd').
                 * So for letters 'a' and 'c' counters will be incremented by 1.
                 * Finally counters will be normalized on total number of replaces.
                 * 
                 * Also, if Id word is longer than all extra symbols will be
                 * assigned as replaced to '_' (nothing).
                 */
                final int length = Math.min (word.length (), expected.length ());
                toCorrect.put (word, expected);
                
                for (int j = 0; j < length; j++) {
                    if (word.charAt (j) != expected.charAt (j)) {
                        String key = "" + word.charAt (j);// + "-" + expected.charAt (j);                        
                        replaces.compute (key, (__, v) -> v == null ? 1 : v + 1);
                        replacesCount += 1;
                    }
                }
                
                for (int j = length; j < word.length (); j++) {
                    final String key = "" + word.charAt (j);
                    replaces.compute (key, (__, v) -> v == null ? 1 : v + 1);
                    replacesCount += 1;
                }
            }
        }
        System.out.println ("Correct words added");
        
        double denominator = replacesCount;
        replaces.forEach ((key, value) -> {
            replaces.compute (key, (__, v) -> v / denominator);
        });
        forest.setStatistics (replaces);
        System.out.println ("Statistics calculated");
        
        final PrintWriter pw = new PrintWriter (directory + fileOUT);
        pw.println ("Id,Expected");
        
        System.out.println ("To fix: " + toCorrect.size () + " words");
        for (int i = 0; i < wordsReader.getRowsNumber (); i++) {
            final String word = wordsReader.get ("Id", i);
            
            String fixed = word;
            if (toCorrect.containsKey (word)) {
                fixed = toCorrect.get (word);
            } else if (!correct.contains (word)) {
                /*
                 * Doing word spellcheck and correction in Prefix Forest
                 * 
                 * As the result we get the sorted list of candidates,
                 * then choose the first (best).
                 */
                fixed = forest.spellcheckWord (word).get (0).F;
            }
            
            pw.println (word + "," + fixed);
            if (i % 250 == 0) {                
                System.out.println (i + ": " + word + " -> " + fixed);
                pw.flush ();
            }
        }
        pw.close ();
        System.out.println ("Spellcheck correction finished");
    }
    
    @SuppressWarnings ("unused")
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
