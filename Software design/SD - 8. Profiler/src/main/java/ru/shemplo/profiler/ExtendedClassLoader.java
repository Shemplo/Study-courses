package ru.shemplo.profiler;

import ru.shemplo.snowball.annot.Snowflake;

@Snowflake (priority = 0)
public class ExtendedClassLoader extends ClassLoader {
    
    public Class <?> defineClass (String name, byte [] bytes) {
        try {
            return defineClass (name, bytes, 0, bytes.length); 
        } catch (Error e) {
            try {
                return loadClass (name);
            } catch (ClassNotFoundException e1) {}
        }
        
        return null;
    }
    
}
