package ru.shemplo.estat.test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import ru.shemplo.estat.EventStatistics;
import ru.shemplo.snowball.annot.Cooler;
import ru.shemplo.snowball.annot.Init;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.utils.fun.StreamUtils;

public class TestEventStatistics extends Snowball {
    
    public static void main (String ... args) { shape (args); }
    
    @Cooler (priority = 1)
    public static SettableTimeProvider newSettableTimeProvider () {
        return new SettableTimeProvider ();
    }
    
    @Init SettableTimeProvider provider;
    @Init EventStatistics statistics;
    
    @Override
    protected void onShaped () {
        try (
            Reader r = new InputStreamReader (System.in, StandardCharsets.UTF_8);
            BufferedReader br = new BufferedReader (r);
        ) {
            System.out.println ("Test initialized");
            while (true) {
                String line = br.readLine ();
                if (line == null) { return; }
                
                List <String> tokens =
                StreamUtils.whilst (StringTokenizer::hasMoreTokens, 
                                    StringTokenizer::nextToken, 
                                    new StringTokenizer (line))
                           .collect (Collectors.toList ());
                try   { handleInput (tokens); } 
                catch (Exception e) { break; }
            }
        } catch (IOException ioe) {
            System.err.println ("Exception occured: " + ioe);
        }
    }
    
    private void handleInput (final List <String> tokens) throws InterruptedException {
        if ("event".equals (tokens.get (0).toLowerCase ())) {
            statistics.registerEvent (tokens.get (1));
        } else if ("time".equals (tokens.get (0).toLowerCase ())) {
            provider.setTime (Long.parseLong (tokens.get (1)));
        } else if ("stat".equals (tokens.get (0).toLowerCase ())) {
            final String event = tokens.get (1);
            System.out.println (statistics.getEventStatisticsByName (event));
        } else if ("stop".equals (tokens.get (0).toLowerCase ())) {
            throw new InterruptedException ();
        }
    }
    
}
