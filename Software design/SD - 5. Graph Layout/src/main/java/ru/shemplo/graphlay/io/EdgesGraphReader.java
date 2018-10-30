package ru.shemplo.graphlay.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import ru.shemplo.graphlay.graph.Graph;

public class EdgesGraphReader implements GraphReader {

    @Override
    public Graph read (String path) throws IOException {
        Path file = Paths.get (path);
        try (
            BufferedReader br = Files.newBufferedReader (file);
        ) {
            
        }
        
        return new Graph ();
    }

}
