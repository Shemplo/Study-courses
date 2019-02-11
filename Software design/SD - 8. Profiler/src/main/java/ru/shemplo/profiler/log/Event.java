package ru.shemplo.profiler.log;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class Event {
    
    private final String className, methodName;
    private final Object [] arguments;
    private final long time;
    
}
