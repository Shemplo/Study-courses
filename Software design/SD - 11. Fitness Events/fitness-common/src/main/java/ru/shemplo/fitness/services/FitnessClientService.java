package ru.shemplo.fitness.services;

import java.io.IOException;
import java.sql.SQLException;

import java.util.List;
import java.util.Map;

import java.util.concurrent.atomic.AtomicInteger;

import ru.shemplo.fitness.AppConfiguration;
import ru.shemplo.fitness.db.DBManager;
import ru.shemplo.fitness.db.DBObjectUnwrapper;
import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.entities.FitnessEvent;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class FitnessClientService {
    
    private DBObjectUnwrapper objectUnwrapper;
    private AppConfiguration configuration;
    private DBManager database;
    
    public FitnessClient createClient (Map <String, String> data) throws IOException {
        Integer nextID;
        try   { nextID = database.runFunction ("SELECT GET_NEXT_ID_FOR ('client');"); } 
        catch (SQLException sqle) { throw new IOException (sqle); }
        
        try { 
            String template = configuration.<String> get ("create-client-data").get ();
            database.update (String.format (template, nextID)); 
        } catch (SQLException sqle) { throw new IOException (sqle); }
        
        return updateClient (nextID, data);
    }
    
    public FitnessClient updateClient (int clientID, Map <String, String> data) throws IOException {        
        try { 
            String template = configuration.<String> get ("update-client-data").get ();
            String [] requests = new String [data.size ()];
            AtomicInteger index = new AtomicInteger ();
            data.forEach ((key, value) -> {
                requests [index.get ()] = String.format (template, clientID, key, value);
                index.incrementAndGet ();
            });
            
            database.update (requests); 
        } catch (SQLException sqle) { throw new IOException (sqle); }
        
        return getClientByID (clientID);
    }
    
    public FitnessClient getClientByID (int clientID) throws IOException {
        try { 
            final String template = configuration.<String> get ("retrieve-client-data").get ();
            String request = String.format (template, clientID);
            
            List <FitnessEvent> sequence = database.retrieve (request, FitnessEvent.class);
            FitnessClient client = objectUnwrapper.unwrap (sequence, FitnessClient.class);
            client.setId (clientID);
            return client;
        } catch (SQLException sqle) { throw new IOException (sqle); }
    }
    
}
