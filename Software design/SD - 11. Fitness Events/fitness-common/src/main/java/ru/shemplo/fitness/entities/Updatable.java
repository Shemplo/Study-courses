package ru.shemplo.fitness.entities;

import java.time.LocalDateTime;

public interface Updatable {
    
    public void setLastTimeUpdated (LocalDateTime dateTime);
    
    public LocalDateTime getLastTimeUpdated ();
    
}
