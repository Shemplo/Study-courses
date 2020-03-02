package ru.shemplo.crypto.lab1;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class RunKasiskiTest {
    
    private static final String KEY = "message";
    
    private static final int [] RANGE = {3, 20};
    
    private static final Map <String, List <Integer>> 
        substrings = new HashMap <> ();
    
    public static void main (String ... args) throws IOException {
        String input = Files.readAllLines (Paths.get ("text.txt")).stream ()
            . collect (Collectors.joining (" ")).toLowerCase ();
        input = RunVigenereCipher.encrypt (input, KEY);
        
        fillPreficies (input, substrings);
        
        List <Integer> possibleLengths = new ArrayList <> ();
        
        for (int i = RANGE [0]; i <= RANGE [1]; i++) {
            List <Integer> distances = new ArrayList <> ();
            
            final int n = i;
            substrings.forEach ((key, entries) -> {
                if (key.length () != n) { return; }
                
                for (int j = 1; j < entries.size (); j++) {
                    distances.add (entries.get (j) - (entries.get (j - 1) + n));
                }
            });
            
            int gcd = distances.size () < 2 
              ? (distances.size () > 0 ? distances.get (0) : -1) 
              : Utils.gcd (distances.get (0), distances.get (1));
            for (int j = 2; j < distances.size (); j++) {
                gcd = Utils.gcd (gcd, distances.get (j));
            }
            
            if (gcd > 1) { possibleLengths.add (gcd); }
        }
        
        possibleLengths = possibleLengths.stream ().distinct ().collect (Collectors.toList ());
        System.out.println ("Possible lengths: " + possibleLengths);
        System.out.println ();
        
        for (int length : possibleLengths) {
            System.out.println ("Try decipher length " + length);
            
            StringBuilder sb = new StringBuilder ();
            for (int i = 0; i < length; i++) {
                var char2amount = countStatistics (input, i, length);
                
                final Double total = char2amount.values ().stream ().mapToDouble (Integer::doubleValue).sum ();
                final Map <Character, Double> profile = char2amount.entrySet ().stream ()
                    . collect (Collectors.toMap (Entry::getKey, e -> e.getValue () / total));
                
                //System.out.println (profile);
                /*
                Character letter = profile.values ().stream ().sorted (Comparator.reverseOrder ()).limit (1)//.peek (System.out::println)
                        .map (RunKasiskiTest::getCharacter).findFirst ().get ();*/
                Character popular = profile.entrySet ().stream ()
                        . sorted (Comparator.<Entry <Character, Double>, Double> comparing (e -> e.getValue ()).reversed ())
                        //. peek (System.out::println)
                        . map (Entry::getKey).findFirst ().get ();
                sb.append ((char) (popular - 'e'));
            }
            
            System.out.println ("Probably key is similar to: " + sb.toString ());
        }
    }
    
    private static void fillPreficies (String input, Map <String, List <Integer>> map) {
        for (int i = RANGE [0]; i <= RANGE [1]; i++) {
            for (int j = 0; j <= input.length () - i; j++) {
                String substring = input.substring (j, j + i);
                
                map.putIfAbsent (substring, new ArrayList <> ());
                map.get (substring).add (j);
            }
        }
    }
    
    private static Map <Character, Integer> countStatistics (String input, int offset, int period) {
        final Map <Character, Integer> map = new HashMap <> ();
        for (int i = offset; i < input.length (); i += period) {
            map.compute (input.charAt (i), (__, v) -> v == null ? 1 : v + 1);
        }
        
        return map;
    }
    
    private static final Map <Character, Double> ENG_STATISTICS = new HashMap <> ();
    
    static {
        ENG_STATISTICS.put ('a', 8.167);    
        ENG_STATISTICS.put ('b', 1.492);    
        ENG_STATISTICS.put ('c', 2.202);    
        ENG_STATISTICS.put ('d', 4.253);    
        ENG_STATISTICS.put ('e', 12.702);   
        ENG_STATISTICS.put ('f', 2.228);    
        ENG_STATISTICS.put ('g', 2.015);    
        ENG_STATISTICS.put ('h', 6.094);    
        ENG_STATISTICS.put ('i', 6.966);    
        ENG_STATISTICS.put ('j', 0.153);    
        ENG_STATISTICS.put ('k', 1.292);    
        ENG_STATISTICS.put ('l', 4.025);    
        ENG_STATISTICS.put ('m', 2.406);    
        ENG_STATISTICS.put ('n', 6.749);    
        ENG_STATISTICS.put ('o', 7.507);    
        ENG_STATISTICS.put ('p', 1.929);    
        ENG_STATISTICS.put ('q', 0.095);    
        ENG_STATISTICS.put ('r', 5.987);    
        ENG_STATISTICS.put ('s', 6.327);    
        ENG_STATISTICS.put ('t', 9.356);    
        ENG_STATISTICS.put ('u', 2.758);    
        ENG_STATISTICS.put ('v', 0.978);    
        ENG_STATISTICS.put ('w', 2.560);    
        ENG_STATISTICS.put ('x', 0.150);    
        ENG_STATISTICS.put ('y', 1.994);    
        ENG_STATISTICS.put ('z', 0.077);
    }
    
    private static char getCharacter (double frequence) {
        frequence *= 100;
        //System.out.println (frequence);
        
        char best = 'a'; double diff = 100.0;
        for (var e : ENG_STATISTICS.entrySet ()) {
            if (Math.abs (e.getValue () - frequence) < diff) {
                diff = Math.abs (e.getValue () - frequence);
                best = e.getKey (); 
            }
        }
        
        return best;
    }
    
}
