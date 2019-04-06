package ru.shemplo.fitness.entities;

import java.time.LocalDateTime;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@ToString
@Getter @Setter
@NoArgsConstructor
public class SeasonTicket implements Completable, Updatable, Identifiable { 
    
    private String secret, name;
    
    private Integer id, visits, client;
    
    private LocalDateTime lastTimeUsed, lastTimeUpdated;
    
    public void createTicket (String value, LocalDateTime when) {
        lastTimeUpdated = when;
    }
    
    public void addVisits (Integer delta, LocalDateTime when) {
        changeVisits (Math.abs (delta), when);
    }
    
    public void subtractVisits (Integer delta, LocalDateTime when) {
        changeVisits (-Math.abs (delta), when);
    }
    
    public void changeVisits (Integer delta, LocalDateTime when) {
        visits = visits == null ? delta : visits + delta;
        visits = Math.max (0, visits);
        lastTimeUpdated = when;
        
        if (delta == -1) { lastTimeUsed = when; }
    }
    
    @Override
    public boolean isCompleted () {
        return id     != null && secret != null 
            && client != null && visits != null;
    }
    
}
