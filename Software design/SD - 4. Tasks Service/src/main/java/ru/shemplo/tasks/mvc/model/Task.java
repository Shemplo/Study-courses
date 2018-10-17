package ru.shemplo.tasks.mvc.model;

import java.util.TimeZone;

import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import lombok.Getter;

public class Task {
    
    public static enum TaskStatus {
        
        IN_PROCESS ("in process", ""), 
        DONE       ("done",       "task-done"), 
        FAILED     ("failed",     "task-failed");
        
        private final String PRINT, STYLE;
        
        private TaskStatus (String print, String style) {
            this.PRINT = print;
            this.STYLE = style;
        }
        
        @Override
        public String toString () {
            return PRINT;
        }
        
        public String getStyle () {
            return STYLE;
        }
        
    }
    
    private final DateFormat DATE_FORMAT 
        = new SimpleDateFormat ("HH:mm 'of' dd.MM.yyyy");
    
    @Getter private final String description;
    @Getter private final TaskStatus status;
    @Getter private final Date expire;
    @Getter private final long ID;
    
    public Task (long id, String desc, Date expire, int status) {
        this.description = desc;
        this.ID = id;
        
        if (expire != null) {
            Date now = new Date (System.currentTimeMillis ());
            if (now.after (expire) && status != TaskStatus.DONE.ordinal ()) {
                status = TaskStatus.FAILED.ordinal ();
            }
        }
        
        this.expire = expire;
        this.status = TaskStatus.values () [status];
    }
    
    public Task (long id, String desc, int status) {
        this (id, desc, null, status);
    }
    
    @Override
    public String toString () {
        StringBuilder sb = new StringBuilder ();
        sb.append ("Task `").append (description).append ("` ");
        sb.append ("[").append (status).append ("]");
        if (hasExpireDate ()) {
            sb.append (" (till ").append (DATE_FORMAT.format (expire))
              .append (")");
        }
        return sb.toString ();
    }
    
    public boolean hasExpireDate () {
        return expire != null;
    }
    
    public String getExpireDate () {
        return DATE_FORMAT.format (expire);
    }
    
    public static Task valueFrom (ResultSet result) throws SQLException {
        String description = result.getString ("desc");
        int status = result.getInt ("status");
        long id = result.getLong ("id");
        
        Date date = result.getDate ("expire");
        Time time = result.getTime ("expire");
        if (date != null && time != null) {
            long offset = TimeZone.getDefault ().getRawOffset ();
            Date ts = new Date (date.getTime () + time.getTime () + offset);
            return new Task (id, description, ts, status);
        }
                
        return new Task (id, description, status);
    }
    
}
