package ru.shemplo.profiler.log;

import java.util.Arrays;
import java.util.List;

import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class SynchronizedEventLogger implements EventLogger {

    @Override
    public synchronized void onMethodStarted (String className, String methodName, Object [] args) {
        System.out.println (String.format ("Start of `%s` in `%s`", methodName, className));
        System.out.println (Arrays.toString (args));
    }

    @Override
    public synchronized void onMethodFinished (String className, String methodName) {
        System.out.println (String.format ("End of `%s` in `%s`", methodName, className));
    }

    @Override
    public List <Event> getEvents () {
        return null;
    }
    
}
