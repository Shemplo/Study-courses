package ru.shemplo.tasks.mvc.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import java.sql.ResultSet;
import java.sql.SQLException;

import lombok.Getter;

public class ListOfTasks {

    @Getter private final String header;
    @Getter private final long ID;
    
    private final List <Task> TASKS;
    
    public ListOfTasks (long id, String title, List <Task> listOfTasks) {
        this.TASKS = new ArrayList <> (listOfTasks);
        this.header = title;
        this.ID = id;
    }
    
    @Override
    public String toString () {
        StringBuilder sb = new StringBuilder ();
        sb.append ("List `").append (header).append ("` of ");
        sb.append (TASKS.toString ());
        return sb.toString ();
    }
    
    public ListOfTasks (long id, String title) {
        this (id, title, new ArrayList <> ());
    }
    
    public void addTask (Task task) {
        this.TASKS.add (task);
    }
    
    public List <Task> getTasks () {
        return Collections.unmodifiableList (TASKS);
    }
    
    public static ListOfTasks valueFrom (ResultSet result) throws SQLException {
        String title = result.getString ("title");
        long id = result.getLong ("id");
        
        return new ListOfTasks (id, title);
    }
    
}
