package ru.shemplo.tasks.mvc.cont;

import static ru.shemplo.tasks.mvc.cont.ResponsePresets.*;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import java.util.function.BiFunction;

import java.sql.SQLException;

import org.json.JSONObject;

import ru.shemplo.tasks.db.DBAccess;

public enum AddKind {

    LIST ((db, r) -> {
        @SuppressWarnings ("unused")
        String title = r.getString ("title");
        return done ();
    }, "title"), 
    
    TASK ((db, r) -> {
        long listID = r.getLong ("list");
        
        String description = r.getString ("desc").trim ();
        if (description.length () == 0) {
            return error ("Description can't be empty");
        }
        
        try {
            db.addTask (listID, description, null);
        } catch (SQLException sqle) {
            return error (sqle);
        }
        
        return done ();
    }, "list", "desc", "?expire");
    
    private final BiFunction <DBAccess, JSONObject, JSONObject> handler;
    private final List <String> fields;
    
    private AddKind (BiFunction <DBAccess, JSONObject, JSONObject> handler, String ... fields) {
        this.fields = Collections.unmodifiableList (Arrays.asList (fields));
        this.handler = handler;
    }
    
    public JSONObject handleRequest (DBAccess db, JSONObject input) {
        try {
            RequestUtils.checkFields (input, fields);
        } catch (RuntimeException re) {
            String name = re.getMessage ();
            return error ("Missed required field `" + name + "`");
        }
        
        return handler.apply (db, input);
    }
    
}
