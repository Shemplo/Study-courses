package ru.shemplo.fitness.services;

import java.io.IOException;
import java.sql.SQLException;
import java.sql.Timestamp;

import java.util.ArrayList;
import java.util.List;

import ru.shemplo.fitness.AppConfiguration;
import ru.shemplo.fitness.db.DBManager;
import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.entities.FitnessEvent;
import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class SeasonTicketService {
    
    private AppConfiguration configuration;
    private DBManager database;
    
    public SeasonTicket createTicket (FitnessClient client, String secret, int visits) throws IOException {
        Integer nextID;
        try   { nextID = database.runFunction ("SELECT GET_NEXT_ID_FOR ('ticket');"); } 
        catch (SQLException sqle) { throw new IOException (sqle); }
        
        try { 
            String tCreate = configuration.<String> get ("create-new-ticket")      .get (),
                   tSecret = configuration.<String> get ("set-secret-to-ticket")   .get (),
                   tClient = configuration.<String> get ("set-client-to-ticket")   .get (),
                   tVisits = configuration.<String> get ("change-visits-of-ticket").get ();
            database.update ( // Creating batch of insert requests
                String.format (tCreate, nextID), 
                String.format (tSecret, nextID, secret),
                String.format (tClient, nextID, client.getId ()),
                String.format (tVisits, nextID, "add", visits)
            ); 
        } catch (SQLException sqle) { throw new IOException (sqle); }
        
        SeasonTicket ticket = new SeasonTicket ();
        long time = System.currentTimeMillis ();
        
        ticket.setLastTimeUpdated (new Timestamp (time));
        ticket.setSecret (secret);
        ticket.setVisits (visits);
        ticket.setId (nextID);
        
        return ticket;
    }
    
    public List <SeasonTicket> getAllTickets () throws IOException {
        List <FitnessEvent> events;
        
        try   { events = database.retrieve ("", FitnessEvent.class); } 
        catch (SQLException sqle) { throw new IOException (sqle); }
        
        events.forEach (System.out::println);
        
        return new ArrayList <> ();
    }
    
}
