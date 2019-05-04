package ru.shemplo.infosearch.spellcheck;

import java.io.IOException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import ru.shemplo.infosearch.spellcheck.forest.PrefixForest;
import ru.shemplo.infosearch.spellcheck.io.CSVReader;
import ru.shemplo.infosearch.spellcheck.io.CSVWriter;

public class RunSpellchecker {
    
    private static final String directory = "itmo-spelling-correction-2019/";
    private static final String fileOUT = "fixed.submission.csv";
    private static final String fileIN = "words.freq.csv";
    
    public static void main (String ... args) throws IOException {
        final CSVReader wordsReader = new CSVReader ();
        wordsReader.read (directory + fileIN, ",");
        
        final PrefixForest forest = new PrefixForest ();
        for (int i = 0; i < wordsReader.getRowsNumber (); i++) {
            int freq = Integer.parseInt (wordsReader.get ("Freq", i));
            String word = wordsReader.get ("Id", i);
            forest.addWord (word, freq);
        }
        
        forest.normalize ();
        
        List <List <String>> rows = new ArrayList <> ();
        for (int i = 0; i < wordsReader.getRowsNumber (); i++) {
            final String word = wordsReader.get ("Id", i);
            String fixed = forest.spellcheckWord (word);
            rows.add (Arrays.asList (word, fixed));
        }
        
        final CSVWriter wordsWriter = new CSVWriter (directory + fileOUT, ",");
        wordsWriter.write (Arrays.asList ("Id", "Expected"), rows);
    }
    
}
