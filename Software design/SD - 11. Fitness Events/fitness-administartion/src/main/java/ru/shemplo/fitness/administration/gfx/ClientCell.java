package ru.shemplo.fitness.administration.gfx;

import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.VBox;

import ru.shemplo.fitness.entities.FitnessClient;

public class ClientCell extends ListCell <FitnessClient> {
    
    private FitnessClient client;
    
    public ClientCell (AdminController controller) {
        setOnMouseClicked (me -> {
            if (!MouseButton.PRIMARY.equals (me.getButton ())) {
                return; // Only primary button is allowed
            }
            
            if (isEmpty () || client == null) { return; }
            controller.openClientDetails (client);
        });
    }
    
    @Override
    protected void updateItem (FitnessClient item, boolean empty) {
        super.updateItem (item, empty);
        client = item;
        
        if (item == null || empty) {
            setGraphic (null);
            return;
        }
        
        
        final VBox root = new VBox ();
        root.setPadding (new Insets (5));
        root.setSpacing (5);
        
        final Label name = new Label (item.getFullName ());
        root.getChildren ().add (name);
        
        String clientRemark = item.getRemark ();
        final Label remark = new Label (clientRemark);
        root.getChildren ().add (remark);
        
        setWrapText (true);
        setGraphic (root);
    }
    
}
