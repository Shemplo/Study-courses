package ru.shemplo.fitness.db;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import java.util.Map;

import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class DBObjectUnwrapper {
    
    public <T> T unwrap (Map <String, Object> map, Class <T> type) throws IOException {
        Constructor <T> constructor;
        try { // check that NoArgsConstructor exists
            constructor = type.getConstructor ();
        } catch (NoSuchMethodException nsme) {
            throw new IOException (nsme);
        }
        
        if (!Modifier.isPublic (constructor.getModifiers ())) {
            String message = "Constructor without arguments must have public access";
            throw new IOException (message);
        }
        
        T object;
        try {
            object = constructor.newInstance ();
        } catch (InstantiationException | IllegalArgumentException 
              | IllegalAccessException | InvocationTargetException e) {
            throw new IOException (e);
        }
        
        for (Method method : type.getDeclaredMethods ()) {
            if (!method.getName ().startsWith ("set")) {
                continue;
            }
            
            String name = method.getName ().substring (3)
                        . toLowerCase ();
            if (!map.containsKey (name)) { continue; }
            
            try {
                method.invoke (object, map.get (name));
            } catch (IllegalAccessException | IllegalArgumentException 
                  | InvocationTargetException e) {
                throw new IOException (e);
            }
        }
        
        return object;
    }
    
}
