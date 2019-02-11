package ru.shemplo.profiler.log;

import lombok.Data;
import lombok.NonNull;

@Data
public class Event {
    
    public static enum EventType { START, FINISH }
    
    @NonNull private String className, methodName;
    @NonNull private Object [] arguments;
    @NonNull private EventType eventType;
    @NonNull private Double time;
    private int depth;
    
}
