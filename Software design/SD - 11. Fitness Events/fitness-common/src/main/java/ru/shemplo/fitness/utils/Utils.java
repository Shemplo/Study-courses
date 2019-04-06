package ru.shemplo.fitness.utils;

import java.lang.reflect.Field;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Utils {
    
    public static String generateSecret (int length) {
        return "very secret value";
    }
    
    public static final DateFormat DATETIME_FORMAT = new SimpleDateFormat ("yyyy-MM-dd HH:mm:ss"),
                                   DATE_FORMAT     = new SimpleDateFormat ("yyyy-MM-dd");;
    
    public static Date parseDate (String date) {
        try {
            return DATETIME_FORMAT.parse (date);
        } catch (ParseException pe) {}
        
        try {
            return DATE_FORMAT.parse (date);
        } catch (ParseException pe) {};
        
        return null;
    }
    
    public static <T> Map <String, String> findDiff (T original, T changed) {
        Map <String, String> diff = new HashMap <> ();
        for (Field field : original.getClass ().getDeclaredFields ()) {
            try {
                field.setAccessible (true);
                Object originalValue = field.get (original), clientValue = field.get (changed);
                if (!Objects.equals (originalValue, clientValue) && clientValue != null) {
                    final String name = field.getName ().toLowerCase ();
                    if (!"id".equalsIgnoreCase (name)) { // this is redundant
                        diff.put (name, Objects.toString (clientValue));                        
                    }
                }
            } catch (IllegalArgumentException | IllegalAccessException e) {
                e.printStackTrace ();
            }
        }
        
        return diff;
    }
    
}
