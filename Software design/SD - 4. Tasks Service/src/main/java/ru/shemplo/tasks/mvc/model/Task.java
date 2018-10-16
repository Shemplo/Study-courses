package ru.shemplo.tasks.mvc.model;

import lombok.Getter;

public class Task {
    
    public static enum TaskStatus {
        IN_PROCESS, DONE, FAILED;
    }
    
    @Getter private final String description;
    private final TaskStatus status;
    
    public Task (String desc, int status) {
        this.status = TaskStatus.values () [status];
        this.description = desc;
    }
    
    public boolean isActive () {
        return TaskStatus.IN_PROCESS.equals (status);
    }
    
}
