package ru.shemplo.graphlay;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import ru.shemplo.graphlay.io.EdgesGraphReader;
import ru.shemplo.graphlay.io.GraphReader;
import ru.shemplo.graphlay.io.MatrixGraphReader;

public enum GraphFormat {

    EDGES  (() -> new EdgesGraphReader  ()),
    MATRIX (() -> new MatrixGraphReader ());

    private static final Map <String, GraphFormat> MATCHES = new HashMap <> ();
    
    public static GraphFormat matchOrDeafault (String input, GraphFormat format) {
        if (input == null || input.length () == 0) { 
            return format; 
        }
        
        if (MATCHES.isEmpty ()) {
            for (GraphFormat __ : GraphFormat.values ()) {
                MATCHES.put (__.name ().toLowerCase (), __);
            }
        }
        
        GraphFormat match = MATCHES.get (input.toLowerCase ().replace (' ', '_'));
        if (match != null) { return match; }
        
        return format;
    }
    
    private final Supplier <GraphReader> SUPPLIER;
    
    private GraphFormat (Supplier <GraphReader> supplier) {
        this.SUPPLIER = supplier;
    }
    
    @Override
    public String toString () {
        return name ().toLowerCase ().replace ('_', ' ');
    }
    
    public GraphReader getInstance () {
        return SUPPLIER.get ();
    }
    
    
}
