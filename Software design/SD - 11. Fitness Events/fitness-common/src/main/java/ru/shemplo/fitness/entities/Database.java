package ru.shemplo.fitness.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@ToString
@Getter @Setter
@NoArgsConstructor
public class Database {
    
    private String URL, login, password;
    
}
