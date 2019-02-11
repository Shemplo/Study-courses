package ru.shemplo.profiler;

import ru.shemplo.snowball.annot.Snowflake;

@Snowflake (priority = 0)
public class ExtendedClassLoader extends ClassLoader {
    
    public Class <?> defineClass (String name, byte [] bytes) {
        return defineClass (name, bytes, 0, bytes.length); 
    }
    
}
