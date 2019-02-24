package ru.shemplo.actor.aggregator.gui;

import java.net.URL;

import java.util.ResourceBundle;

import java.util.concurrent.atomic.AtomicInteger;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.MouseButton;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.pattern.Patterns;
import ru.shemplo.actor.aggregator.RunSearchAggregator;
import ru.shemplo.actor.aggregator.engine.JAggregateEngine;
import ru.shemplo.actor.aggregator.engine.units.JSRequest;
import ru.shemplo.actor.aggregator.engine.units.JSResponse;
import ru.shemplo.actor.aggregator.engine.units.JSResponse.JSResponseRow;

public class WindowController implements Initializable {

    private final Props ENGINE_PROPS = Props.create (JAggregateEngine.class);
    private final ActorSystem actors = RunSearchAggregator.getActors ();
    
    private final AtomicInteger requestsCounter = new AtomicInteger ();
    
    @FXML private ListView <JSResponseRow> resultsList;
    @FXML private ListView <JSResponse> historyList;
    
    @FXML private TextField searchQuery;
    @FXML private Button searchButton;
    
    @Override
    public void initialize (URL location, ResourceBundle resources) {
        searchQuery.setOnKeyReleased (ke -> {
            KeyCode code = ke.getCode ();
            if (KeyCode.ENTER.equals (code)) {
                sendSearchRequest ();
            }
        });
        
        searchButton.setOnMouseClicked (me -> {
            MouseButton button = me.getButton ();
            if (MouseButton.PRIMARY.equals (button)) {
                sendSearchRequest ();
            }
        });
    }
    
    private void sendSearchRequest () {
        final String query = searchQuery.getText ().trim ();
        if (query.length () == 0) {
            System.err.println ("Search query is empty");
            System.err.flush ();
            
            return;
        }
        
        ActorRef actor = actors.actorOf (ENGINE_PROPS, getNextActorName ());
        Patterns.ask (actor, new JSRequest (query), 10000L)
                .onComplete (__ -> {
                    System.out.println (__);
                    return __;
                }, actors.dispatcher ());
    }
    
    private String getNextActorName () {
        return String.format ("search-actor-%d", 
            requestsCounter.getAndIncrement ());
    }
    
}
