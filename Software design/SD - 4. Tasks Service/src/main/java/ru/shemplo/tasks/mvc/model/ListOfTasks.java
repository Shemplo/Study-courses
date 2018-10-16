package ru.shemplo.tasks.mvc.model;

import java.util.Collections;
import java.util.List;

import lombok.Getter;

public class ListOfTasks {

    @Getter private final String header;
    @Getter private List <Task> tasks;
    @Getter private final long ID;
    
    public ListOfTasks (long id, String header, List <Task> tasks) {
        this.tasks = Collections.unmodifiableList (tasks);
        this.header = header;
        this.ID = id;
    }
    
}
