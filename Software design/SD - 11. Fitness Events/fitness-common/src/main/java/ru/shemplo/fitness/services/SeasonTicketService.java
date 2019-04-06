package ru.shemplo.fitness.services;

import java.io.IOException;
import java.sql.SQLException;
import java.time.LocalDateTime;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import java.util.concurrent.atomic.AtomicBoolean;

import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.entities.FitnessEvent;
import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.stuctures.Pair;

@Snowflake
public class SeasonTicketService extends AbsService <SeasonTicket> {
    
    public SeasonTicketService () {
        super (SeasonTicket.class);
    }
    
    @Override
    public List <FitnessEvent> getAllEventsAfter (LocalDateTime dateTime) throws IOException {
        List <FitnessEvent> events = super.getAllEventsAfter (dateTime);
        if (events.size () > 0) {            
            statisticsModule.onNewTicketEvents (events);
        }
        
        return events;
    }
    
    /**
     * Returns all {@link SeasonTicket}s that belong to specified {@link FitnessClient}
     * 
     * @param client representation of real world client entity
     * 
     * @return list of {@link SeasonTicket tickets} referenced to the on {@link FitnessClient client}
     * 
     * @throws IOException if failed to retrieve data from DB
     * 
     * @see FitnessClientService#getClientByID(int)
     * 
     */
    public List <SeasonTicket> getTicketsByClient (FitnessClient client) throws IOException {
        String template = configuration.<String> get ("retrieve-p-client-tickets").get ();
        String request = String.format (template, client.getId ());
        
        List <FitnessEvent> events;
        try   { events = database.retrieve (request, FitnessEvent.class); } 
        catch (SQLException sqle) { throw new IOException (sqle); }
        
        final Integer clientID = client.getId ();
        return eventsToInstances (events).stream ()
             . filter (t -> t.getClient ().equals (clientID))
             . collect (Collectors.toList ());
    }
    
    public Pair <List <SeasonTicket>, Boolean> updateTickets (List <SeasonTicket> tickets,
            LocalDateTime lastUpdate, int current) throws IOException {
        Map <Integer, List <FitnessEvent>> events = getAllEventsAfter (lastUpdate).stream ()
          . collect (Collectors.groupingBy (FitnessEvent::getObjectId));
        Map <Integer, SeasonTicket> ticketss = tickets.stream ()
          . collect (Collectors.toMap (SeasonTicket::getId, __ -> __));
        AtomicBoolean currentUpdated = new AtomicBoolean (false);
        final List <SeasonTicket> toAdd = new ArrayList <> ();
        
        events.forEach ((id, eventss) -> {
            if (ticketss.containsKey (id)) {
                final SeasonTicket ticket = ticketss.get (id);
                objectUnwrapper.unwrapTo (eventss, ticket);
                if (current == ticket.getClient ()) { 
                    currentUpdated.set (true); 
                }
            } else {
                final SeasonTicket ticket = new SeasonTicket ();
                objectUnwrapper.unwrapTo (eventss, ticket);
                if (current == ticket.getClient ()) { 
                    currentUpdated.set (true); 
                }
                toAdd.add (ticket);
            }
        });
        
        List <SeasonTicket> toAddFiltered = toAdd.stream ()
           . filter (SeasonTicket::isCompleted)
           . collect (Collectors.toList ());
             
        return Pair.mp (toAddFiltered, currentUpdated.get ());
    }
    
    public void subtractVisits (SeasonTicket ticket, int amount) throws IOException {
        final String template = configuration.<String> get ("create-new-event").get ();
        final String objectName = TOKEN.getSimpleName ().toLowerCase ();
        
        final String request = String.format (template, objectName, ticket.getId (), 
                                                 "visits", "subtract", "" + amount);
        try   { database.update (request); }
        catch (SQLException sqle) { throw new IOException (sqle); }
    }
    
}
