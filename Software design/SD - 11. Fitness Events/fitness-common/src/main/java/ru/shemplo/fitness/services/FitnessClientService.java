package ru.shemplo.fitness.services;

import java.io.IOException;
import java.time.LocalDateTime;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import java.util.concurrent.atomic.AtomicBoolean;

import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.entities.FitnessEvent;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.stuctures.Pair;

@Snowflake
public class FitnessClientService extends AbsService <FitnessClient> {
    
    public FitnessClientService () {
        super (FitnessClient.class);
    }
    
    public Pair <List <FitnessClient>, Boolean> updateClients (List <FitnessClient> clients, 
            LocalDateTime lastUpdate, int current) throws IOException {
        Map <Integer, List <FitnessEvent>> events = getAllEventsAfter (lastUpdate).stream ()
          . collect (Collectors.groupingBy (FitnessEvent::getObjectId));
        Map <Integer, FitnessClient> clientss = clients.stream ()
          . collect (Collectors.toMap (FitnessClient::getId, __ -> __));
        AtomicBoolean currentUpdated = new AtomicBoolean (false);
        final List <FitnessClient> toAdd = new ArrayList <> ();
        
        events.forEach ((id, eventss) -> {
            if (clientss.containsKey (id)) {
                final FitnessClient client = clientss.get (id);
                objectUnwrapper.unwrapTo (eventss, client);
                
                if (current == id) { currentUpdated.set (true); }
            } else {
                final FitnessClient client = new FitnessClient ();
                objectUnwrapper.unwrapTo (eventss, client);
                toAdd.add (client);
            }
        });
        
        List <FitnessClient> toAddFiltered = toAdd.stream ()
           . filter (FitnessClient::isCompleted)
           . collect (Collectors.toList ());
        
        return Pair.mp (toAddFiltered, currentUpdated.get ());
    }
    
}
