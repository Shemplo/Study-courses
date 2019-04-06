package ru.shemplo.fitness.administration.gfx;

import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.VBox;

import ru.shemplo.fitness.entities.SeasonTicket;

public class TicketCell extends ListCell <SeasonTicket> {
    
    private SeasonTicket ticket;
    
    public TicketCell (ClientController controller) {
        setOnMouseClicked (me -> {
            if (!MouseButton.PRIMARY.equals (me.getButton ())) {
                return; // Only primary button is allowed
            }
            
            if (isEmpty () || ticket == null) { return; }
            controller.onTicketSelected (ticket);
        });
    }
    
    @Override
    protected void updateItem (SeasonTicket item, boolean empty) {
        super.updateItem (item, empty);
        ticket = item;
        
        if (item == null || empty) {
            setGraphic (null);
            return;
        }
        
        
        final VBox root = new VBox ();
        root.setPadding (new Insets (5));
        root.setSpacing (5);
        
        final Label name = new Label (item.getName ());
        root.getChildren ().add (name);
        
        String visits = String.format ("Посещений: %d", item.getVisits ());
        final Label remark = new Label (visits);
        root.getChildren ().add (remark);
        
        setWrapText (true);
        setGraphic (root);
    }
    
}
