package ru.shemplo.estat;

import java.util.Random;

import ru.shemplo.estat.gen.EventGenerator;
import ru.shemplo.snowball.annot.Cooler;
import ru.shemplo.snowball.annot.processor.Snowball;

public class RunEventStatistics extends Snowball {
    
    public static void main (String ... args) { shape (args); }
    
    @Cooler
    public static Random newRandom () {
        return new Random ();
    }
    
    //@Init 
    private EventGenerator generator;
    
    @Override
    protected void onShaped () { generator.startGenerator (); }
    
}
