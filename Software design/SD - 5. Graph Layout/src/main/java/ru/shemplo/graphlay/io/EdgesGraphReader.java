package ru.shemplo.graphlay.io;

import java.util.List;

import ru.shemplo.graphlay.graph.Graph;

public class EdgesGraphReader extends AbsGraphReader {

    @Override
    public void readUnknownLine (Graph graph, List <String> values) {
        int from = Integer.parseInt (values.get (0)), 
              to = Integer.parseInt (values.get (1));
        graph.addEdge (from, to);
    }

}
