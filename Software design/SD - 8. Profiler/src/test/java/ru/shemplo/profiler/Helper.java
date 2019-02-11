package ru.shemplo.profiler;

import java.util.Random;
import java.util.stream.Stream;

public class Helper {
    
    public String randomString (int length) {
        StringBuilder sb = new StringBuilder ();
        Random r = new Random ();
        Stream.iterate (0, i -> i + 1).limit (length)
              .map     (__ -> r.nextInt ('z' - 'a' + 1))
              .map     (i -> 'a' + i)
              .forEach (i -> sb.append ((char) i.intValue ()));
        return sb.toString ();
    }
    
    public String reverse (String string) {
        if (string.length () < 2) {
            return string;
        }
        
        int length = string.length ();
        return string.charAt (length - 1) 
             + reverse (string.substring (1, length - 1)) 
             + string.charAt (0);
    }
    
}
