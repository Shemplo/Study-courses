package ru.shemplo.estat;

import ru.shemplo.estat.gen.EventGenerator;
import ru.shemplo.snowball.annot.Init;
import ru.shemplo.snowball.annot.processor.Snowball;

public class RunEventStatistics extends Snowball {
    
    public static void main (String ... args) { shape (args); }
    
    @Init private EventGenerator generator;
    
    @Override
    protected void onShaped () {
        System.out.println (System.nanoTime () / 1_000_000_000_000L);
        System.out.println (generator);
        generator.startGenerator ();
    }
    
}
