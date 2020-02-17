package ru.shemplo.infosearch.spellcheck;

import java.io.IOException;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import ru.shemplo.infosearch.spellcheck.io.CSVReader;

public class FilesComaprator {
    
    private static final String directory = "itmo-spelling-correction-2019/";
    private static final String fileOUT = "fixed.submission.csv";
    private static final String fileIN2 = "public.freq.csv";
    private static final String fileIN = "words.freq.csv";
    
    public static void main (String ... args) throws IOException {
        final CSVReader wordsReader = new CSVReader ();
        wordsReader.read (directory + fileIN, ",");
        System.out.println ("IN file read");
        
        Set <String> words = new HashSet <> ();
        for (int i = 0; i < wordsReader.getRowsNumber (); i++) {
            words.add (wordsReader.get ("Id", i));
        }
        System.out.println ("Words: " + words.size ());
        
        final CSVReader answersReader = new CSVReader ();
        answersReader.read (directory + fileOUT, ",");
        System.out.println ("OUT file read");
        
        final Map <String, String> corrections = new HashMap <> ();
        for (int i = 0; i < answersReader.getRowsNumber (); i++) {
            String correction = answersReader.get ("Expected", i);
            String word = answersReader.get ("Id", i);
            
            corrections.putIfAbsent (word, correction);
        }
        System.out.println ("Answers: " + corrections.size ());
        
        words.removeAll (corrections.keySet ());
        System.out.println ("Missed: " + words.size ());
        System.out.println (words);
    }
    
}
