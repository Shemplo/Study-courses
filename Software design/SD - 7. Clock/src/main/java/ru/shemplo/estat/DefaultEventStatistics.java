package ru.shemplo.estat;

import java.util.List;

import ru.shemplo.estat.clock.TimeProvider;
import ru.shemplo.snowball.annot.Cooler;
import ru.shemplo.snowball.annot.Init;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.stuctures.Pair;

@Snowflake
public class DefaultEventStatistics implements EventStatistics {
    
    @Cooler
    public static DefaultEventStatistics __ () {
        return new DefaultEventStatistics ();
    }
    
    @Init private TimeProvider provider;

    @Override
    public void registerEvent (String event) {
        
    }

    @Override
    public int getEventStatisticsByName (String name) {
        return 0;
    }

    @Override
    public List <Pair <String, Integer>> getAllEventsStatistics () {
        return null;
    }
    
}
