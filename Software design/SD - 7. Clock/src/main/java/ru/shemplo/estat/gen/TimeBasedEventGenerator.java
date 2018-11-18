package ru.shemplo.estat.gen;

import java.util.Random;

import java.util.concurrent.atomic.AtomicLong;

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
    @Init private Random random;
    
    private Thread thread;
    
    private final Runnable THREAD_TASK = () -> {
        final long tmp = provider.getCurrentTime ();
        AtomicLong previousTime = new AtomicLong (tmp);
        
        while (true) {
            long time  = provider.getCurrentTime (),
                 delta = time - previousTime.get ();
            final int id = 1 + random.nextInt (10);
            if (random.nextDouble () < 1.0 * delta / 20000) {
                statistics.registerEvent ("Event " + id);
                previousTime.set (time);
                /*
                System.out.println (String.format ("Event `%s` ocurred\n%s (total %-3d)", 
                        "Event " + id, new Date (time).toString (),
                        statistics.getEventStatisticsByName ("Event "+ id)));
                */
            } else {
                try   { Thread.sleep (10); } 
                catch (InterruptedException ie) {
                    return;
                }
            }
        }
    };
    
    @Override
    public void startGenerator () {
        if (thread == null) {
            thread = new Thread (THREAD_TASK);
        }
        
        thread.start ();
    }
    
}
