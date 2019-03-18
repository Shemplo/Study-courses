package ru.shemplo.fitness.entities;

import java.sql.Timestamp;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@ToString
@Getter @Setter
@NoArgsConstructor
public class SeasonTicket { 
    
    private String secret;
    
    private Integer id, visits;
    
    private Timestamp lastTimeUsed, lastTimeUpdated;
    
}
