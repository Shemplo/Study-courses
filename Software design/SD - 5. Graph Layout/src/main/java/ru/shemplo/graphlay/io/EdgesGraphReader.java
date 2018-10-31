package ru.shemplo.graphlay.io;

import static ru.shemplo.snowball.utils.fun.StreamUtils.whilst;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import ru.shemplo.graphlay.graph.Graph;

public class EdgesGraphReader implements GraphReader {

    @Override
    public Graph read (String path) throws IOException {
        Path file = Paths.get (path);
        Graph graph = new Graph ();
        
        try (
            BufferedReader br = Files.newBufferedReader (file);
        ) {
            
            String line = null;
            while ((line = br.readLine ()) != null) {
                StringTokenizer st = new StringTokenizer (line);
                List <String> values = whilst (t -> t.hasMoreTokens (), 
                                               t -> t.nextToken (), st)
                                     . collect (Collectors.toList ());
                if (values.size () == 0) { continue; } // Skip empty line
                String first = values.get (0).toLowerCase ();
                if ("orientated:".equals (first)) {
                    boolean orientated = "1".equals (values.get (1));
                    graph.setOrientated (orientated);
                } else if ("vertexes:".equals (first)) {
                    int vertexes = Integer.parseInt (values.get (1));
                    for (int i = 0; i < vertexes; i++) {
                        graph.addVertex (i + 1);
                    }
                } else {
                    int from = Integer.parseInt (first), 
                        to = Integer.parseInt (values.get (1));
                    graph.addEdge (from, to);
                }
            }
        }
        
        return graph;
    }

}
