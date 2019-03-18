package ru.shemplo.fitness.administration.gfx;

import java.net.URL;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class AdminApplication extends Application {

    private static final String FXML_TEMPLATE = "/fxml/admin.fxml";
    
    @Override
    public void start (Stage stage) throws Exception {
        final URL url = getClass ().getResource (FXML_TEMPLATE);
        final Scene scene = new Scene (FXMLLoader.load (url));
        
        stage.setTitle ("Fitness Events - administration");
        stage.setScene (scene);
        stage.show ();
    }
    
}
