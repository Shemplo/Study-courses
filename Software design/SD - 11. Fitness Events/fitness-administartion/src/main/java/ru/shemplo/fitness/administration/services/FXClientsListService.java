package ru.shemplo.fitness.administration.services;

import static ru.shemplo.fitness.services.AbsService.*;

import java.time.LocalDateTime;

import java.util.Collections;
import java.util.List;

import javafx.application.Platform;
import javafx.concurrent.Service;
import javafx.concurrent.Task;
import javafx.scene.control.ListView;

import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.services.FitnessClientService;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowflakeInitializer;
import ru.shemplo.snowball.stuctures.Pair;

public class FXClientsListService extends Service <Boolean> {
    
    private FitnessClientService clientService;
    
    private final ListView <FitnessClient> listView;
    
    public FXClientsListService (final ListView <FitnessClient> listView) {
        SnowflakeInitializer.initFields (Snowball.getContext (), this);
        this.listView = listView;
    }
    
    @Override
    protected Task <Boolean> createTask () {
        return new Task <Boolean> () {
            
            @Override 
            protected Boolean call () throws Exception {
                FitnessClient selected = listView.getSelectionModel ().getSelectedItem ();
                int selection = selected != null ? selected.getId () : -1;
                
                final List <FitnessClient> clients = listView.getItems ();
                LocalDateTime from = clients.stream ().map (FitnessClient::getLastTimeUpdated)
                                   . sorted (Collections.reverseOrder ()).findFirst ()
                                   . orElse (getStartDate ());
                Pair <List <FitnessClient>, Boolean> result = clientService
                                 .updateClients (clients, from, selection);
                Platform.runLater (() -> {
                    listView.getItems ().addAll (result.F);
                });
                
                return result.S;
            }
            
        };
    }
    
}
