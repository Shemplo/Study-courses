package ru.shemplo.fitness.db;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ru.shemplo.fitness.entities.FitnessEvent;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class DBObjectUnwrapper {
    
    public <T> T unwrap (Map <String, Object> map, Class <T> type) throws IOException {
        final T object = makeRawInstance (type);
        
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
    
    public <T> T unwrap (List <FitnessEvent> sequence, Class <T> type) throws IOException {
        final T object = makeRawInstance (type);
        
        Map <String, Method> setters = new HashMap <> ();
        for (Method method : type.getDeclaredMethods ()) {
            String name = method.getName ().toLowerCase ();
            setters.put (name, method);
        }
        
        sequence.stream ().forEach (event -> {
            final String action = event.getPropertyAction (),
                         name   = event.getPropertyName   ()
                                . replace     ("_", "")
                                . toLowerCase ();
            
            if (!setters.containsKey (action + name)) {
                return; // no setter found in type
            }
            
            Method method = setters.get (action + name);
            if (method.getParameterTypes ().length != 1) {
                return; // wrong signature of this setter
            }
            
            Class <?> argument = method.getParameterTypes () [0];
            if (String.class.isAssignableFrom (argument)) {
                try {
                    method.invoke (object, event.getPropertyValue ());
                } catch (IllegalAccessException | IllegalArgumentException 
                      | InvocationTargetException e) {
                    e.printStackTrace ();
                    return; // failed to call this setter
                }
            } else if (Number.class.isAssignableFrom (argument)) {
                final String TNAME = argument.getSimpleName ();
                
                try {
                    Method parser = argument.getDeclaredMethod ("parse" + TNAME, String.class);
                    final Object value = parser.invoke (null, event.getPropertyValue ());
                    
                    method.invoke (object, value);
                } catch (IllegalAccessException | IllegalArgumentException 
                      | InvocationTargetException | NoSuchMethodException 
                      | SecurityException e) {
                    e.printStackTrace ();
                    return; // failed to call this setter
                }
            } else {
                System.err.println ("Unknow type");
            }
        });
        
        return object;
    }
    
    private <T> T makeRawInstance (Class <T> type) throws IOException {
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
        
        return object;
    }
    
}
