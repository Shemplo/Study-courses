package ru.shemplo.fitness.administration.services;

import java.util.Map;

import javafx.concurrent.Service;
import javafx.concurrent.Task;

import ru.shemplo.fitness.administration.gfx.ClientController;
import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.services.FitnessClientService;
import ru.shemplo.fitness.utils.Utils;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowflakeInitializer;

public class FXClientUpdateService extends Service <FitnessClient> {

    private FitnessClientService clientService;
    
    private final FitnessClient current, changed;
    
    public FXClientUpdateService (ClientController controller, FitnessClient current,
            FitnessClient changed) {
        SnowflakeInitializer.initFields (Snowball.getContext (), this);
        this.current = current; this.changed = changed;
        
        setOnSucceeded (wse -> {
            if (getValue () == null) { return; }
            
            if (current == null) {                
                controller.getAdminController ().closeDetails ();
            }
            
            controller.getAdminController ().getClientsList ()
                      .refresh ();
        });
        
        setOnFailed (__ -> getException ().printStackTrace ());
    }
    
    @Override 
    protected Task <FitnessClient> createTask () {
        return new Task <FitnessClient> () {

            @Override protected FitnessClient call () throws Exception {
                Map <String, String> diff = Utils.findDiff (current, changed);
                
                if (current.getId () == null) { // new user
                    if (!diff.containsKey ("firstname") || !diff.containsKey ("lastname")) {
                        System.err.println ("Client name or last name is missed");
                        return null; // not enough data to create new client
                    }
                    
                    FitnessClient client = clientService.create (diff);
                    return client;
                }
                
                if (diff.size () == 0) {
                    System.err.println ("No changes found");
                    return current;
                }
                
                return clientService.updateData (current.getId (), diff);
            }
            
        };
    }
    
}
