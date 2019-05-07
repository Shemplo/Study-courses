package ru.shemplo.infosearch.spellcheck.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class CSVReader {
    
    private List <String> titles      = new ArrayList <> ();
    private List <List <String>> rows = new ArrayList <> ();
    
    public List <String> getTitles () {
        return Collections.unmodifiableList (titles);
    }
    
    public int getRowsNumber () {
        //System.out.println (rows.stream ().map (row -> row.get (0)).collect (Collectors.toSet ()).size ());
        return rows.size ();
    }
    
    public void read (String filepath, String delimeter) throws IOException {
        Path path = Paths.get (filepath);
        titles.clear (); rows.clear ();
        
        try (
            BufferedReader br = Files.newBufferedReader (path);
        ) {
            String [] tokens = br.readLine ().split (delimeter);
            titles.addAll (Arrays.asList (tokens));
            
            String line = null;
            while ((line = br.readLine ()) != null) {
                tokens = line.split (delimeter);
                rows.add (Arrays.asList (tokens));
            }
        }
    }
    
    public String get (String column, int row) {
        int columnIndex = titles.indexOf (column);
        if (columnIndex == -1) {
            final String message = "Wrong column name";
            throw new IllegalStateException (message);
        }
        
        return rows.get (row).get (columnIndex);
    }
    
}
