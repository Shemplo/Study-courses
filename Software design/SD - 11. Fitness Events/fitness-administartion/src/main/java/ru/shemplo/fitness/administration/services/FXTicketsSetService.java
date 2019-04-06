package ru.shemplo.fitness.administration.services;

import static ru.shemplo.fitness.services.AbsService.*;

import java.time.LocalDateTime;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javafx.concurrent.Service;
import javafx.concurrent.Task;
import javafx.scene.control.ListView;

import ru.shemplo.fitness.administration.gfx.AdminController;
import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.fitness.services.SeasonTicketService;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowflakeInitializer;
import ru.shemplo.snowball.stuctures.Pair;

public class FXTicketsSetService extends Service <Boolean> {

    private SeasonTicketService ticketService;
    
    private final ListView <FitnessClient> listView;
    private final AdminController controller;
    
    public FXTicketsSetService (final ListView <FitnessClient> listView,
            AdminController controller) {
        SnowflakeInitializer.initFields (Snowball.getContext (), this);
        this.listView = listView; this.controller = controller;
    }
    
    @Override
    protected Task <Boolean> createTask () {
        return new Task <Boolean> () {

            @Override protected Boolean call () throws Exception {
                FitnessClient selected = listView.getSelectionModel ().getSelectedItem ();
                int selection = selected != null ? selected.getId () : -1;
                
                List <SeasonTicket> tickets = new ArrayList <> (controller.getTicketsPool ());
                LocalDateTime from = tickets.stream ().map (SeasonTicket::getLastTimeUpdated)
                                   . sorted (Collections.reverseOrder ()).findFirst ()
                                   . orElse (getStartDate ());
                Pair <List <SeasonTicket>, Boolean> result = ticketService
                                .updateTickets (tickets, from, selection);
                synchronized (controller.getTicketsPool ()) {
                    controller.getTicketsPool ().addAll (result.F);
                }
                
                return result.S;
            }
            
        };
    }
    
}
