package ru.shemplo.crypto.lab5;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RunSmallExponentHack {
    
    private static final BigInteger EXP = BigInteger.valueOf (3);
    
    private static final String MESSAGE = "Test 123";
    
    // :NOTE: before start change bounds of primary generator UPPER to 256, LOWER to 0
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        final List <Tup4 <BigInteger [], BigInteger, BigInteger, BigInteger>> ciphers = new ArrayList <> ();
        final var knownNs = new HashSet <BigInteger> ();
        
        while (ciphers.size () < 20) {
            final var cipher = RunRSACipher.encrypt (MESSAGE.toCharArray (), EXP);
            if (cipher.T4.compareTo (BigInteger.valueOf (500)) <= 0
                    && !knownNs.contains (cipher.T4)) {
                knownNs.add (cipher.T4);
                ciphers.add (cipher);
            }
        }
        
        BigInteger bounds = ciphers.stream ().map (t -> t.T4).limit (3).reduce (BigInteger.ONE, BigInteger::multiply);
        
        for (int i = 0; i < ciphers.get (0).T1.length; i++) { 
            final int index = i;
            
            final var options = ciphers.stream ().<Set <BigInteger>> map (cipher -> {
                final var code = cipher.T1 [index];
                final var steps = bounds.divide (cipher.T4).intValue ();
                final var step = cipher.T4;
                
                return Stream.iterate (code, it -> it.add (step)).limit (steps)
                     . collect (Collectors.toSet ());
            }).reduce ((res, cur) -> {
                res.retainAll (cur);
                return res;
            }).get ();
            
            if (options.size () != 1) {
                final var rest = options.stream ().sorted ().distinct ().limit (20).collect (Collectors.toList ());
                System.out.println ();
                System.out.println ("Failed to hack: " + rest);
            }
            
            final var m = options.stream ().map (v -> (char) Math.round (Math.pow (v.doubleValue (), 1.0 / 3)))
                    . findFirst ().orElseThrow ();
            System.out.print (m);
        }
    }
    
}
