package ru.shemplo.estat.gen;

import ru.shemplo.estat.EventStatistics;
import ru.shemplo.estat.clock.TimeProvider;
import ru.shemplo.snowball.annot.Cooler;
import ru.shemplo.snowball.annot.Init;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class TimeBasedEventGenerator implements EventGenerator {
    
    @Cooler
    public static TimeBasedEventGenerator getInstance () {
        return new TimeBasedEventGenerator ();
    }
    
    @Init private EventStatistics statistics;
    @Init private TimeProvider provider;
    
    @Override
    public void startGenerator () {
        
    }
    
}
