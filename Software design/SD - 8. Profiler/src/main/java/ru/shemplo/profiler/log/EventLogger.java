package ru.shemplo.profiler.log;

import java.util.List;

public interface EventLogger {
    
    public void onMethodStarted (String className, String methodName, Object [] args);
    
    public void onMethodFinished (String className, String methodName);
    
    public List <Event> getEvents ();
    
}
