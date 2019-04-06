package ru.shemplo.fitness.db;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.time.LocalDate;
import java.time.LocalDateTime;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ru.shemplo.fitness.entities.FitnessEvent;
import ru.shemplo.fitness.entities.Identifiable;
import ru.shemplo.fitness.entities.Updatable;
import ru.shemplo.fitness.utils.Utils;
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
    
    public <T extends Updatable & Identifiable> T unwrapTo (List <FitnessEvent> sequence, T instance) {
        Map <String, Method> setters = new HashMap <> ();
        for (Method method : instance.getClass ().getDeclaredMethods ()) {
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
            Object [] arguments = new Object [method.getParameterTypes ().length];
            
            for (int i = 0; i < arguments.length; i++) {
                Class <?> argument = method.getParameterTypes () [i];
                Object value = null;
                
                if (i == 0 && String.class.isAssignableFrom (argument)) {
                    value = event.getPropertyValue ();
                } else if (i == 0 && Number.class.isAssignableFrom (argument)) {
                    String TNAME = argument.getSimpleName ();
                    
                    try {
                        if (TNAME.equals ("Integer")) { TNAME = "Int"; }
                        Method parser = argument.getDeclaredMethod ("parse" + TNAME, String.class);
                        value = parser.invoke (null, event.getPropertyValue ());
                    } catch (IllegalAccessException | IllegalArgumentException 
                          | InvocationTargetException | NoSuchMethodException 
                          | SecurityException e) {
                        e.printStackTrace ();
                        return; // failed to call this setter
                    }
                } else if (LocalDate.class.isAssignableFrom (argument)) {
                    if   (i == 0) { value = LocalDate.parse (event.getPropertyValue ()); } 
                    else { value = event.getDate ().toLocalDateTime ().toLocalDate (); }
                } else if (LocalDateTime.class.isAssignableFrom (argument)) {
                    if   (i == 0) { value = LocalDateTime.parse (event.getPropertyValue ()); } 
                    else { value = event.getDate ().toLocalDateTime (); }
                } else if (Date.class.isAssignableFrom (argument)) {
                    if   (i == 0) { value = Utils.parseDate (event.getPropertyValue ()); } 
                    else { value = event.getDate (); }
                } else {
                    System.err.println ("Unknow type: " + argument);
                    return; // unsupported type of field, no chances to insert
                }
                
                arguments [i] = value;
            }
            
            try   { method.invoke (instance, arguments); } 
            catch (IllegalAccessException | IllegalArgumentException 
                | InvocationTargetException e) {
                return; // failed to call this setter
            }
            
            instance.setId (event.getObjectId ());
            //instance.setLastTimeUpdated (event.getDate ());
            instance.setLastTimeUpdated (event.getDate ().toLocalDateTime ());
        });
        
        return instance;
    }
    
    public <T extends Updatable & Identifiable> T unwrap (List <FitnessEvent> sequence, Class <T> type) 
            throws IOException {
        return unwrapTo (sequence, makeRawInstance (type));
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
