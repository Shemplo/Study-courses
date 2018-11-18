package ru.shemplo.estat;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import ru.shemplo.estat.clock.TimeProvider;
import ru.shemplo.snowball.annot.Cooler;
import ru.shemplo.snowball.annot.Init;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.stuctures.Pair;
import ru.shemplo.snowball.utils.time.TimeDelta;
import ru.shemplo.snowball.utils.time.TimeDelta.TDUnit;

@Snowflake
public class DefaultEventStatistics implements EventStatistics {
    
    @Cooler
    public static DefaultEventStatistics __ () {
        return new DefaultEventStatistics ();
    }
    
    @Init private TimeProvider provider;

    private final Map <String, LinkedList <Long>> 
        EVENTS = new HashMap <> ();
    
    @Override
    public synchronized void registerEvent (String event) {
        EVENTS.putIfAbsent     (event, new LinkedList <> ());
        EVENTS.get (event).add (provider.getCurrentTime ());
    }

    @Override
    public synchronized int getEventStatisticsByName (String name) {
        if (EVENTS.get (name) == null) { return 0; }
        
        LinkedList <Long> sequence = EVENTS.get (name);
        clearEventsSequense (sequence);
        return sequence.size ();
    }

    @Override
    public synchronized List <Pair <String, Integer>> getAllEventsStatistics () {
        return EVENTS.entrySet ().stream ()
             . map  (Pair::fromMapEntry)
             . peek (p -> clearEventsSequense (p.S))
             . map  (p -> Pair.mp (p.F, p.S.size ()))
             . collect (Collectors.toList ());
    }
    
    private void clearEventsSequense (LinkedList <Long> sequence) {
        while (!sequence.isEmpty ()) {
            final long current = provider.getCurrentTime (),
                       event   = sequence.peek ();
            TimeDelta delta = TimeDelta.valueOf (current - event);
            if (delta.floorTo (TDUnit.MIN) >= 60) {
                sequence.poll ();
                continue;
            }

            break;
        }
    }
    
}
