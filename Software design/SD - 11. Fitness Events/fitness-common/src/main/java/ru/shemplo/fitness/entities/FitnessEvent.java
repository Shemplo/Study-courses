package ru.shemplo.fitness.entities;

import java.sql.Timestamp;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@ToString
@Getter @Setter
@NoArgsConstructor
public class FitnessEvent {
    
    private Integer   id,             // 
                      objectId;       //
    private String    objectClass,    // 
                      propertyName,   //
                      propertyAction, //
                      propertyValue;  //
    private Timestamp date;           //
    
}
