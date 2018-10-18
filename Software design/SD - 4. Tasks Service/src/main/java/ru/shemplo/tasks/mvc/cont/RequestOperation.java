package ru.shemplo.tasks.mvc.cont;

import static ru.shemplo.tasks.mvc.model.Task.TaskStatus.*;
import static ru.shemplo.tasks.db.DBAccess.*;
import static ru.shemplo.tasks.mvc.cont.ResponsePresets.*;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import java.util.function.BiFunction;

import java.sql.SQLException;
import java.text.ParseException;

import org.json.JSONObject;

import ru.shemplo.tasks.db.DBAccess;
import ru.shemplo.tasks.mvc.model.Task;

public enum RequestOperation {

    ADD_LIST ((db, r) -> {
        String title = r.getString ("title").trim ();
        if (title.length () == 0) {
            return error ("Title can't be empty");
        }
        
        try {
            db.addList (title);
        } catch (SQLException sqle) {
            return error (sqle);
        }
        
        return done ();
    }, "title"), 
    
    DELETE_LIST ((db, r) -> {
        long listID = r.getLong ("list");
        
        try {
            db.deleteList (listID);
        } catch (SQLException sqle) {
            return error (sqle);
        }
        
        return done ();
    }, "list"),
    
    ADD_TASK ((db, r) -> {
        long listID = r.getLong ("list");
        
        String description = r.getString ("desc").trim ();
        if (description.length () == 0) {
            return error ("Description can't be empty");
        }
        
        Date expire = null;
        if (r.has ("expireDate")) {
            String expireDate = r.getString ("expireDate");
            if (r.has ("expireTime")) {
                expireDate += " " + r.getString ("expireTime");
            } else { expireDate += " 00:00"; }
            expireDate = expireDate.trim ();
            
            try {
                expire = SQL_FORMAT.parse (expireDate);
                
                if (!expire.after (new Date (System.currentTimeMillis ()))) {
                    return error ("This task is already failed");
                }
            } catch (ParseException pe) {
                return error (pe);
            }
        }
        
        try {
            db.addTask (listID, description, expire);
        } catch (SQLException sqle) {
            return error (sqle);
        }
        
        return done ();
    }, "list", "desc", "?expireDate", "?expireTime"),
    
    DELETE_TASK ((db, r) -> {
        long taskID = r.getLong ("task");
        
        try {
            db.deleteTask (taskID);
        } catch (SQLException sqle) {
            return error (sqle);
        }
        
        return done ();
    }, "task"),
    
    UPDATE_TASK ((db, r) -> {
        long taskID = r.getLong ("task");
        Task task = db.getTask (taskID);
        
        if (task == null) {
            return error ("Task not found");
        }
        
        if (FAILED.equals (task.getStatus ())) {
            return error ("Task is failed (this status can't be undone)");
        }
        
        try {
            int status = (task.getStatus ().ordinal () + 1) % 2;
            db.setTaskStatus (taskID, status);
        } catch (SQLException sqle) {
            return error (sqle);
        }
        
        return done ();
    }, "task");
    
    private final BiFunction <DBAccess, JSONObject, JSONObject> handler;
    private final List <String> fields;
    
    private RequestOperation (BiFunction <DBAccess, JSONObject, JSONObject> handler, String ... fields) {
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
