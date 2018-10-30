package ru.shemplo.graphlay.io;

import java.io.IOException;

import ru.shemplo.graphlay.graph.Graph;

public interface GraphReader {

    public Graph read (String path) throws IOException;
    
}
