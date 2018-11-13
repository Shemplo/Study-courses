package ru.shemplo.estat;

import java.util.List;

import ru.shemplo.snowball.stuctures.Pair;

public interface EventStatistics {
    
    // the same is incEvent (String name);
    void registerEvent (String event);
    
    /**
     * Returns RPM (requests per minute) for the last hour for specified event.
     * 
     * @param name
     * 
     * @return
     * 
     */
    int getEventStatisticsByName (String name);
    
    /**
     * Returns RPM (requests per minute) for the last hour for all events.
     * 
     * @return
     * 
     */
    List <Pair <String, Integer>> getAllEventsStatistics ();
    
}
