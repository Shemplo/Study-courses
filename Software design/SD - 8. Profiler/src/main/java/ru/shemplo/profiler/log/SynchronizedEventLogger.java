package ru.shemplo.profiler.log;

import static ru.shemplo.profiler.log.Event.EventType.*;
import static ru.shemplo.profiler.Profiler.*;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class SynchronizedEventLogger implements EventLogger {

    @Getter private final List <Event> events = new ArrayList <> ();
    
    @Override
    public synchronized void onMethodStarted (String className, String methodName, Object [] args) {
        events.add (new Event (toName (className), methodName, args, 
                               START, (double) System.nanoTime ()));
    }

    @Override
    public synchronized void onMethodFinished (String className, String methodName) {
        events.add (new Event (toName (className), methodName, new Object [] {}, 
                               FINISH, (double) System.nanoTime ()));
    }
    
}
