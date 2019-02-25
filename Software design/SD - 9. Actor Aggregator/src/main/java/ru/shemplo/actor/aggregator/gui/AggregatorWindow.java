package ru.shemplo.actor.aggregator.gui;

import java.net.URL;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
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
        
        URL styles = ClassLoader.getSystemResource ("css/aggregator.css");
        rootParent.getStylesheets ().add (styles.toExternalForm ());
        stage.getIcons ().add (new Image ("/gfx/www.png"));
        
        stage.setTitle ("Search snowball");
        stage.setScene (scene);
        stage.show ();
        
        stage.setOnCloseRequest (we -> {
            actors.terminate ();
        });
    }
    
}
