package ru.shemplo.fitness.administration.gfx;

import java.io.IOException;
import java.net.URL;

import java.util.*;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.concurrent.Service;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.ScrollPane;
import javafx.scene.input.MouseButton;
import javafx.util.Duration;

import lombok.Getter;
import ru.shemplo.fitness.administration.services.FXClientsListService;
import ru.shemplo.fitness.administration.services.FXTicketsSetService;
import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.entities.SeasonTicket;

public class AdminController implements Initializable {
    
    private static final Comparator <FitnessClient> LEXICOGRAPHIC_ORDER 
          = Comparator.comparing(FitnessClient::getFullName);
    
    @Getter
    private final Set <SeasonTicket> ticketsPool = new HashSet <> ();
    
    @FXML private Button addClient, openStatistics;

    @FXML private ScrollPane details;
    
    @Getter @FXML 
    private ListView <FitnessClient> clientsList;
    
    private Service <Boolean> clientsListService, ticketsSetService;
    
    private final Timeline clientListServiceCaller = new Timeline (
        new KeyFrame (Duration.millis (0), __ -> {
            if (!clientsListService.isRunning ()) {
                clientsListService.restart ();
            }
        }),
        new KeyFrame (Duration.seconds (2.5))
    );
    private final Timeline ticketsSetServiceCaller = new Timeline (
        new KeyFrame (Duration.millis (0), __ -> {
            if (!ticketsSetService.isRunning ()) {
                ticketsSetService.restart ();
            }
        }),
        new KeyFrame (Duration.seconds (1.5))
    );
    
    @Override
    public final void initialize (URL location, ResourceBundle resources) {
        clientsList.setCellFactory (__ -> new ClientCell (this));
        clientsList.editableProperty ().set (false);
        
        clientsListService = new FXClientsListService (clientsList);
        clientsListService.setOnSucceeded (__ -> {
            ClientController controller = currentClient;
            if (clientsListService.getValue () && controller != null) {
                controller.currentClientUpdated ();
            }
            
            clientsList.getItems ().sort (LEXICOGRAPHIC_ORDER);
        });
        
        clientListServiceCaller.setCycleCount (Timeline.INDEFINITE);
        clientListServiceCaller.playFromStart ();
        
        ticketsSetService = new FXTicketsSetService (clientsList, this);
        ticketsSetService.setOnSucceeded (__ -> {
            ClientController controller = currentClient;
            if (ticketsSetService.getValue () && controller != null) {
                controller.currentClientUpdated ();
            }
        });
        
        ticketsSetServiceCaller.setCycleCount (Timeline.INDEFINITE);
        ticketsSetServiceCaller.playFromStart ();
        
        addClient.setOnMouseClicked (me -> {
            if (MouseButton.PRIMARY.equals (me.getButton ())) {
                openClientDetails (null);
            }
        });
        
        openStatistics.setOnMouseClicked (me -> {
            if (MouseButton.PRIMARY.equals (me.getButton ())) {
                openStatisticsDetails ();
            }
        });
    }
    
    @Getter
    private volatile ClientController currentClient;
    
    public synchronized void openClientDetails (FitnessClient client) {
        closeDetails ();
        
        FitnessClient insert = Optional.ofNullable (client).orElse (new FitnessClient ());
        final ClientController controller = new ClientController (this, insert);
        final URL url = getClass ().getResource ("/fxml/client.fxml");
        final FXMLLoader loader = new FXMLLoader (url);
        loader.setController (controller);
        currentClient = controller;
        
        try   { details.setContent(loader.load ()); } 
        catch (IOException e) { e.printStackTrace(); }
    }
    
    public synchronized void openStatisticsDetails () {
        closeDetails ();
        
        final URL url = getClass ().getResource ("/fxml/statistics.fxml");
        final FXMLLoader loader = new FXMLLoader (url);
        
        try   { details.setContent(loader.load ()); } 
        catch (IOException e) { e.printStackTrace(); }
    }
    
    public synchronized void closeDetails () {
        if (currentClient != null) { // stopping previous opened controller
            try   { currentClient.close (); } 
            catch (Exception e) {}
        }
        
        details.setContent (null);
    }
    
}
