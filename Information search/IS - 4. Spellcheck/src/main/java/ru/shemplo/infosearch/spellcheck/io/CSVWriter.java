package ru.shemplo.infosearch.spellcheck.io;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;

import java.util.List;
import java.util.StringJoiner;

public class CSVWriter {
    
    private final String delimiter;
    private final Path path;
    
    public CSVWriter (String filepath, String delimiter) {
        path = Paths.get (filepath);
        this.delimiter = delimiter;
    }
    
    public void write (List <String> titles, List <List <String>> rows) throws IOException {
        try (
            PrintWriter pw = new PrintWriter (path.toFile ());
        ) {
            StringJoiner sj = new StringJoiner (delimiter);
            titles.forEach    (sj::add);
            pw.println (sj.toString ());
            
            for (List <String> row : rows) {
                sj = new StringJoiner (delimiter);
                row.stream ().forEach (sj::add);
                pw.println (sj.toString ());
            }
        }
    }
    
}
