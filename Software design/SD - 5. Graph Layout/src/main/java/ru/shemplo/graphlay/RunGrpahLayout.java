package ru.shemplo.graphlay;

import java.util.Arrays;
import java.util.List;
import java.util.MissingFormatArgumentException;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import ru.shemplo.graphlay.io.Parameter;
import ru.shemplo.graphlay.io.ParametersData;

public class RunGrpahLayout {

    public static void main (String ... args) throws Exception {
        List <Parameter> missed = checkParameters (ParametersData.parse (args));
        if (missed != null && missed.size () > 0) {
            StringJoiner sj = new StringJoiner (", ");
            missed.forEach (p -> sj.add ("[" + p.KEY + " = " + p + "]"));
            
            StringBuilder sb = new StringBuilder ();
            sb.append ("Missed arguments: ").append (sj.toString ());
            throw new MissingFormatArgumentException (sb.toString ());
        }
    }
    
    private static List <Parameter> checkParameters (ParametersData params) {
        return Arrays.asList (Parameter.values ()).stream ()
             . filter (p -> p.IS_REQUIRED && params.getValue (p) == null)
             . collect (Collectors.toList ());
    }
    
}
