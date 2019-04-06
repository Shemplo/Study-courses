package ru.shemplo.fitness.services;

import java.io.IOException;
import java.sql.SQLException;
import java.time.LocalDateTime;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import java.util.concurrent.atomic.AtomicInteger;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import ru.shemplo.fitness.AppConfiguration;
import ru.shemplo.fitness.db.DBManager;
import ru.shemplo.fitness.db.DBObjectUnwrapper;
import ru.shemplo.fitness.entities.*;
import ru.shemplo.fitness.statistics.StatisticsModule;

@RequiredArgsConstructor
public abstract class AbsService <T extends Identifiable & Completable & Updatable> {
    
    protected DBObjectUnwrapper objectUnwrapper;
    protected StatisticsModule statisticsModule;
    protected AppConfiguration configuration;
    protected DBManager database;
    
    protected final Class <T> TOKEN;
    
    public T create (Map <String, String> data) throws IOException {
        Integer nextID;
        
        String objectClass = TOKEN.getSimpleName ().toLowerCase ();
        String request = String.format ("SELECT GET_NEXT_ID_FOR ('%s');", objectClass);
        try   { nextID = database.runFunction (request); } 
        catch (SQLException sqle) { throw new IOException (sqle); }
        
        try { 
            String template = configuration.<String> get ("create-data-by-type").get ();
            database.update (String.format (template, objectClass, nextID)); 
        } catch (SQLException sqle) { throw new IOException (sqle); }
        
        return updateData (nextID, data);
    }
    
    public T updateData (int objectID, Map <String, String> data) throws IOException {        
        try { 
            String template = configuration.<String> get ("update-data-by-type").get ();
            String objectClass = TOKEN.getSimpleName ().toLowerCase ();
            String [] requests = new String [data.size ()];
            AtomicInteger index = new AtomicInteger ();
            data.forEach ((key, value) -> {
                requests [index.get ()] = String.format (template, 
                               objectClass, objectID, key, value);
                index.incrementAndGet ();
            });
            
            database.update (requests); 
        } catch (SQLException sqle) { throw new IOException (sqle); }
        
        return getByID (objectID);
    }
    
    @Getter protected static final LocalDateTime startDate 
          = LocalDateTime.parse ("2019-01-01T00:00:00");
    
    public List <FitnessEvent> getAllEvents () throws IOException {
        return getAllEventsAfter (startDate);
    }
    
    public List <FitnessEvent> getAllEventsAfter (LocalDateTime dateTime) throws IOException {
        List <FitnessEvent> events;
        
        final String template = configuration.<String> get ("retrieve-all-by-type-after").get ();
        final String objectName = TOKEN.getSimpleName ().toLowerCase ();
        final String date = dateTime.toString ().replace ('T', ' ');
        //final String date = Utils.DATETIME_FORMAT.format (dateTime);
        
        final String request = String.format (template, objectName, date);
        try   { events = database.retrieve (request, FitnessEvent.class); }
        catch (SQLException sqle) { throw new IOException (sqle); }
        
        return events;
    }
    
    public List <T> getAll () throws IOException {
        return getAllAfter (startDate); // No events earlier can be
    }
    
    public List <T> getAllAfter (LocalDateTime dateTime) throws IOException {
        return eventsToInstances (getAllEventsAfter (dateTime));
    }
    
    public T getByID (int id) throws IOException {
        final String template = configuration.<String> get ("retrieve-data-by-type-id").get ();
        final String objectName = TOKEN.getSimpleName ().toLowerCase ();
        String request = String.format (template, objectName, id);
        
        List <FitnessEvent> events;
        try   { events = database.retrieve (request, FitnessEvent.class); } 
        catch (SQLException sqle) { throw new IOException (sqle); }
        
        T instance = objectUnwrapper.unwrap (events, TOKEN);
        if (!instance.isCompleted ()) { return null; }
        
        return instance;
    }
    
    protected List <T> eventsToInstances (final List <FitnessEvent> events) {
        final Map <Integer, List <FitnessEvent>> eventsByTickets = events.stream ()
                . collect (Collectors.groupingBy (FitnessEvent::getObjectId));
        final List <T> instances = new ArrayList <> ();
            
        eventsByTickets.forEach ((id, sequence) -> {
            try {
                final T instance = objectUnwrapper.unwrap (sequence, TOKEN);
                if (instance.isCompleted ()) { instances.add (instance); }
            } catch (IOException e) {}
        });
        
        return instances;
    }
    
}
