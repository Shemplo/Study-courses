package ru.shemplo.fitness.services;

import java.util.ArrayList;
import java.util.List;

import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class SeasonTicketService {
    
    public List <SeasonTicket> getAllTickets () {
        return new ArrayList <> ();
    }
    
}
