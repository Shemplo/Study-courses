package ru.shemplo.graphlay.graph;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import lombok.Getter;
import ru.shemplo.graphlay.gfx.GraphRender;
import ru.shemplo.snowball.stuctures.Pair;

public class Graph {

    private final List <Pair <Integer, Integer>> EDGES = new ArrayList <> ();
    private final Set <Integer> VERTEXES = new HashSet <> ();
    @Getter private boolean isOrientated = false;
    
    @Override
    public String toString () {
        StringBuilder sb = new StringBuilder ();
        sb.append ("Vertexes: ").append (VERTEXES).append ("\n");
        sb.append ("Edges: ").append (EDGES).append ("\n");
        sb.append ("Orientated: " + isOrientated ());
        return sb.toString ();
    }
    
    public void addVertex (int vertex) {
        if (VERTEXES.contains (vertex)) {
            System.err.println ("Vertex already exists");
        }
        
        VERTEXES.add (vertex);
    }
    
    public void addEdge (int from, int to) {
        if (!VERTEXES.contains (from)) {
            System.err.println ("Vertex (from) " + from + " doesn't exist");
        }
        if (!VERTEXES.contains (to)) {
            System.out.println ("Vertex (to) " + to +  " doesn't exist");
        }
        
        EDGES.add (Pair.mp (from, to));
    }
    
    public void setOrientated (boolean orientated) {
        isOrientated = true;
    }
    
    private double getRadiansFor (int number, int total) {
        return (2 * Math.PI / total) * number;
    }
    
    public void render (GraphRender render) {
        Map <Integer, Pair <Double, Double>> positions = new HashMap <> ();
        List <Integer> vertexes = new ArrayList <> (VERTEXES);
        Collections.sort (vertexes);
        
        for (Integer vertex : vertexes) {
            double angle  = getRadiansFor (vertex, vertexes.size ()),
                   radius = 100.0;
            double x = Math.cos (angle) * radius,
                   y = Math.sin (angle) * radius,
                   r = 5;
            
            positions.put (vertex, Pair.mp (x, y));
            render.fillCircle (x, y, r);
        }
        
        // for not orientated
        for (Pair <Integer, Integer> edge : EDGES) {
            if (!positions.containsKey (edge.F) 
            || !positions.containsKey (edge.S)) {
                continue;
            }
            
            Pair <Double, Double> from = positions.get (edge.F),
                                  to   = positions.get (edge.S);
            render.strokeLine (from.F, from.S, to.F, to.S);
        }
    }
    
}
