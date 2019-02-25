package ru.shemplo.actor.aggregator.gui;

import static java.awt.Desktop.*;

import java.awt.Desktop.Action;
import java.io.IOException;
import java.net.URISyntaxException;

import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.layout.VBox;

import ru.shemplo.actor.aggregator.engine.units.JSResponse.JSResponseRow;

public class ResponseListCell extends ListCell <JSResponseRow> {
    
    @Override
    protected void updateItem (JSResponseRow item, boolean empty) {
        super.updateItem (item, empty);
        if (item == null || empty) {
            setGraphic (null);
            return;
        }
        
        final VBox root = new VBox ();
        root.setPadding (new Insets (5));
        
        final Label title = new Label (item.getTitle ());
        title.getStyleClass ().add ("response-title");
        root.getChildren ().add (title);
        title.setWrapText (true);
        
        final Label link = new Label (item.getLink ().toString ());
        link.getStyleClass ().add ("response-link");
        root.getChildren ().add (link);
        link.setWrapText (true);
        
        link.setOnMouseClicked (me -> {
            if (isDesktopSupported () && getDesktop ().isSupported (Action.BROWSE)) {
                try   { getDesktop ().browse (item.getLink ().toURI ()); } 
                catch (IOException ioe) {} catch (URISyntaxException urise) {}
            }
        });
        
        String value = item.getDescription ();
        if (value != null && value.trim ().length () > 0) {
            final Label description = new Label (value.trim ());
            description.setPadding (new Insets (10, 0, 0, 0));
            root.getChildren ().add (description);
            description.setWrapText (true);
        }
        
        final Label source = new Label ("found by " + 
                      item.getSource ().getTitle ());
        source.getStyleClass ().add ("response-src");
        root.getChildren ().add (source);
        
        setWrapText (true);
        setGraphic (root);
    }
    
}
