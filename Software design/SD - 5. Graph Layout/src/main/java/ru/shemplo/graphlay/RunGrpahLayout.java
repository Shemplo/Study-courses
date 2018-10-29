package ru.shemplo.graphlay;

import static ru.shemplo.graphlay.io.Parameter.*;
import static ru.shemplo.graphlay.GraphFormat.*;
import java.util.Arrays;
import java.util.List;
import java.util.MissingFormatArgumentException;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import ru.shemplo.graphlay.graph.Graph;
import ru.shemplo.graphlay.io.GraphReader;
import ru.shemplo.graphlay.io.Parameter;
import ru.shemplo.graphlay.io.ParametersData;

public class RunGrpahLayout {

    public static void main (String ... args) throws Exception {
        ParametersData data = ParametersData.parse (args);
        List <Parameter> missed = checkParameters (data);
        if (missed != null && missed.size () > 0) {
            StringJoiner sj = new StringJoiner (", ");
            missed.forEach (p -> sj.add ("[" + p.KEY + " = " + p + "]"));
            
            StringBuilder sb = new StringBuilder ();
            sb.append ("Missed arguments: ").append (sj.toString ());
            throw new MissingFormatArgumentException (sb.toString ());
        }
 
        GraphFormat format = matchOrDeafault (data.getValue (FORMAT), null);
        if (format == null) {
            StringBuilder sb = new StringBuilder ();
            sb.append ("Unknown format of graph. Valid values: ")
              .append (Arrays.toString (GraphFormat.values ()));
            throw new MissingFormatArgumentException (sb.toString ());
        }
        
        
        
        GraphReader reader = format.getInstance ();
        Graph graph = reader.read (data.getValue (GRAPH_FILE));
        
        
        graph.render ();
    }
    
    private static List <Parameter> checkParameters (ParametersData params) {
        return Arrays.asList (Parameter.values ()).stream ()
             . filter (p -> p.IS_REQUIRED && params.getValue (p) == null)
             . collect (Collectors.toList ());
    }
    
}
