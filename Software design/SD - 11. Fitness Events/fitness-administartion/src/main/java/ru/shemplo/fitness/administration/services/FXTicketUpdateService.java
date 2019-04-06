package ru.shemplo.fitness.administration.services;

import java.util.Map;

import javafx.concurrent.Service;
import javafx.concurrent.Task;

import ru.shemplo.fitness.administration.gfx.ClientController;
import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.fitness.services.SeasonTicketService;
import ru.shemplo.fitness.utils.Utils;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowflakeInitializer;

public class FXTicketUpdateService extends Service <SeasonTicket> {

    private SeasonTicketService ticketService;
    
    private final SeasonTicket current, changed;
    
    public FXTicketUpdateService (ClientController controller, SeasonTicket current,
            SeasonTicket changed) {
        SnowflakeInitializer.initFields (Snowball.getContext (), this);
        this.current = current; this.changed = changed;
        
        setOnSucceeded (wse -> {
            if (getValue () == null) { return; }
            
            controller.getTicketDetails ().setDisable (true);
            controller.currentClientUpdated ();
            
            controller.getAdminController ().getTicketsPool ().forEach (System.out::println);
        });
        
        setOnFailed (__ -> getException ().printStackTrace ());
    }
    
    @Override 
    protected Task <SeasonTicket> createTask () {
        return new Task <SeasonTicket> () {

            @Override protected SeasonTicket call () throws Exception {
                Map <String, String> diff = Utils.findDiff (current, changed);
                
                if (current.getId () == null) { // new user
                    System.out.println (diff);
                    if (!diff.containsKey ("name") || !diff.containsKey ("secret")) {
                        System.err.println ("Ticket name or secret is missed");
                        return null; // not enough data to create new client
                    }
                    
                    SeasonTicket ticket = ticketService.create (diff);
                    return ticket;
                }
                
                if (diff.size () == 0) {
                    System.err.println ("No changes found");
                    return current;
                }
                
                if (diff.containsKey ("visits")) {
                    int visits = Integer.parseInt (diff.get ("visits"));
                    if (visits < 0) {
                        System.err.println ("Number of visits can't be negative");
                        return null; // Invalid passing data (visits amount)
                    }
                }
                
                // Unfortunately incremental change value of `visits` field
                // is impossible and from this client it will be set only
                // through `set` action in event
                return ticketService.updateData (current.getId (), diff);
            }
            
        };
    }
    
}
