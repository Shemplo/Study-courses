package ru.shemplo.graphlay.graph;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
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
        double r = 25.0;
        
        Random random = new Random ();
        for (Integer vertex : vertexes) {
            double angle  = getRadiansFor (vertex, vertexes.size ()),
                   radius = 25.0 + random.nextInt (250);
            double x = Math.cos (angle) * radius,
                   y = Math.sin (angle) * radius;
            
            positions.put (vertex, Pair.mp (x, y));
        }
        
        // for not orientated
        render.setStroke (new Color (1.0f, 0, 0.25f, 0.55f));
        render.setLineWidth (3d);
        for (Pair <Integer, Integer> edge : EDGES) {
            if (!positions.containsKey (edge.F) 
            || !positions.containsKey (edge.S)) {
                continue;
            }
            
            Pair <Double, Double> from = positions.get (edge.F),
                                  to   = positions.get (edge.S);
            render.strokeLine (from.F, from.S, to.F, to.S);
        }
        
        render.setFill (new Color (1.0f, 0f, 0f, 0.65f));
        for (Integer vertex : vertexes) {
            Pair <Double, Double> v = positions.get (vertex);
            render.fillCircle (v.F, v.S, r);
        }
    }
    
}
