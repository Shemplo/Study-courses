package ru.shemplo.fitness.administration.gfx;

import java.io.IOException;
import java.net.URL;

import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.layout.HBox;

public class AdminController implements Initializable {
    
    //@FXML private BorderPane clientDetails;
    @FXML private HBox clientDetails;
    
    @Override
    public void initialize (URL location, ResourceBundle resources) {
        final URL url = getClass ().getResource ("/fxml/client.fxml");
        try {
            clientDetails.getChildren ().add (FXMLLoader.load (url));
            //clientDetails.setRight (FXMLLoader.load (url));
        } catch (IOException e) { e.printStackTrace(); }
    }
    
}
