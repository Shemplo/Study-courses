package ru.shemplo.tasks.mvc.cont;

import java.util.List;

import java.text.DateFormat;
import java.text.SimpleDateFormat;

import org.json.JSONObject;

public class RequestUtils {

    public static final DateFormat REQUEST_FORMAT = new SimpleDateFormat ("");
    
    public static void checkFields (JSONObject input, List <String> fields) {
        fields.forEach (p -> {
            boolean isRequired = p.charAt (0) != '?';
            String name = isRequired ? p : p.substring (1);
            
            if (isRequired && !input.has (name)) {
                throw new RuntimeException (name);
            }
        });
    }
    
}
