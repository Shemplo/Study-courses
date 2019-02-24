package ru.shemplo.actor.aggregator.gui;

import java.net.URL;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import akka.actor.ActorSystem;
import ru.shemplo.actor.aggregator.RunSearchAggregator;

public class AggregatorWindow extends Application {

    private Parent rootParent;
    private Scene scene;
    
    private final ActorSystem actors = RunSearchAggregator
                                     . getActors ();
    
    @Override
    public void start (Stage stage) throws Exception {
        URL layoutFile = this.getClass ().getResource ("/fxml/aggregator.fxml");
        rootParent = FXMLLoader.load (layoutFile);
        scene = new Scene (rootParent);
        
        stage.setTitle ("Search snowball");
        stage.setScene (scene);
        stage.show ();
        
        stage.setOnCloseRequest (we -> {
            actors.terminate ();
        });
    }
    
}
