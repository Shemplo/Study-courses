package ru.shemplo.tasks.mvc.model;

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
    
    @Getter private final String description;
    @Getter private final TaskStatus status;
    
    public Task (String desc, int status) {
        this.status = TaskStatus.values () [status];
        this.description = desc;
    }
    
    public boolean isActive () {
        return TaskStatus.IN_PROCESS.equals (status);
    }
    
}
