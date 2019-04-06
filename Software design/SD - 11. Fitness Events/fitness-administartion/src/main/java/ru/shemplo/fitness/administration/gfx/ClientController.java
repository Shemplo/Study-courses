package ru.shemplo.fitness.administration.gfx;

import java.io.IOException;
import java.net.URL;
import java.time.LocalDateTime;

import java.util.List;
import java.util.Optional;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.VBox;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import ru.shemplo.fitness.administration.services.FXClientUpdateService;
import ru.shemplo.fitness.administration.services.FXTicketUpdateService;
import ru.shemplo.fitness.entities.FitnessClient;
import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.fitness.services.SeasonTicketService;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowballContext;

@Getter
@RequiredArgsConstructor
public class ClientController implements Initializable, AutoCloseable {

    private final AdminController adminController;
    private final FitnessClient client;
    
    private FitnessClient changedClient = new FitnessClient ();
    
    @FXML private Button saveClientDetails, resetClientDetails,
                         saveTicketDetails, addTicket,
                         genTicketName, genFakeVisit;
    
    @FXML private ListView <SeasonTicket> ticketsList;
    
    @Getter @FXML private VBox ticketDetails;
    
    @FXML private TextField 
        idF, 
        
        firstNameF, secondNameF, lastNameF,
        organizationF, positionF,
        countryF, stateF, cityF, districtF,
        phoneF, emailF, homePageF,
        remarkF,
        
        ticketNameF, ticketSecretF,
        ticketVisitsF, ticketLastUsedF;
    
    @FXML private ChoiceBox <String> sexF;
    
    @FXML private DatePicker birthdayF;
    
    @Override
    public void initialize (URL location, ResourceBundle resources) {
        bindFields (); initializeFields ();
        
        ticketsList.setCellFactory (__ -> new TicketCell (this));
        
        saveClientDetails.setOnMouseClicked (me -> {
            if (!MouseButton.PRIMARY.equals (me.getButton ())) {
                return;
            }
            
            new FXClientUpdateService (this, client, changedClient)
            . restart ();
        });
        
        resetClientDetails.setOnMouseClicked (me -> {
            if (MouseButton.PRIMARY.equals (me.getButton ())) {
                initializeFields (); // hard override of all fields
            }
        });
        
        saveTicketDetails.setOnMouseClicked (me -> {
            if (!MouseButton.PRIMARY.equals (me.getButton ())) {
                return;
            }
            
            new FXTicketUpdateService (this, ticket, changedTicket)
            . restart ();
        });
        
        addTicket.setOnMouseClicked (me -> {
            if (MouseButton.PRIMARY.equals (me.getButton ())) {
                onTicketSelected (new SeasonTicket ());
            }
        });
        
        genTicketName.setOnMouseClicked (me -> {
            if (!MouseButton.PRIMARY.equals (me.getButton ())) {
                return;
            }
            
            int number = ticketsList.getItems ().stream ()
                       . map      (SeasonTicket::getName)
                       . map      (name -> name.replaceAll ("[^\\d]", ""))
                       . map      (String::trim)
                       . filter   (str -> !str.contains (" "))
                       . mapToInt (Integer::parseInt)
                       .max       ().orElse (0) + 1;
            ticketNameF.setText (String.format ("Season ticket #%d", number));
        });
        
        genFakeVisit.setOnMouseClicked (me -> {
            if (!MouseButton.PRIMARY.equals (me.getButton ())) {
                return;
            }
            
            SnowballContext context     = Snowball.getContext ();
            SeasonTicketService service = context.getSnowflakeFor (SeasonTicketService.class);
            
            if (ticket.getId () != null && ticket.getVisits () > 0) {
                new Thread (() -> {
                    try {
                        service.subtractVisits (ticket, 1);
                    } catch (IOException ioe) {
                        throw new RuntimeException (ioe);
                    }
                }).start ();
            }
        });
    }
    
    public void currentClientUpdated () {
        initializeFields ();
    }
    
    private SeasonTicket ticket, changedTicket = new SeasonTicket ();
    
    public synchronized void onTicketSelected (SeasonTicket ticket) {
        this.ticket = ticket;
        
        Platform.runLater (() -> {
            ticketDetails.setDisable (false);
            initializeFields ();
        });
    }
    
    public void initializeFields () {
        idF.          setText  ( "" + Optional.ofNullable (client.getId ()).orElse (-1));
        firstNameF.   setText  (Optional.ofNullable (client.getFirstName ()).orElse (""));
        secondNameF.  setText  (Optional.ofNullable (client.getSecondName ()).orElse (""));
        lastNameF.    setText  (Optional.ofNullable (client.getLastName ()).orElse (""));
        organizationF.setText  (Optional.ofNullable (client.getOrganization ()).orElse (""));
        positionF.    setText  (Optional.ofNullable (client.getPosition ()).orElse (""));
        countryF.     setText  (Optional.ofNullable (client.getCountry ()).orElse (""));
        stateF.       setText  (Optional.ofNullable (client.getState ()).orElse (""));
        cityF.        setText  (Optional.ofNullable (client.getCity ()).orElse (""));
        districtF.    setText  (Optional.ofNullable (client.getDistrict ()).orElse (""));
        phoneF.       setText  (Optional.ofNullable (client.getPhone ()).orElse (""));
        emailF.       setText  (Optional.ofNullable (client.getEmail ()).orElse (""));
        homePageF.    setText  (Optional.ofNullable (client.getHomePage ()).orElse (""));
        remarkF.      setText  (Optional.ofNullable (client.getRemark ()).orElse (""));
        birthdayF.    setValue (Optional.ofNullable (client.getBirthday ()).orElse (null));
        sexF.         setValue (Optional.ofNullable (client.getSex ()).orElse (null));
        
        List <SeasonTicket> tickets = adminController.getTicketsPool ().stream ()
                                    . filter  (t -> t.getClient ().equals (client.getId ()))
                                    . collect (Collectors.toList ());
        ticketsList.setItems (FXCollections.observableArrayList (tickets));
        ticketsList.getItems ().sort ((a, b) -> a.getName ().compareTo (b.getName ()));
        ticketsList.refresh  ();
        
        SeasonTicket ticket = this.ticket;
        if (ticket != null) {
            ticketNameF.        setText (Optional.ofNullable (ticket.getName ()).orElse (""));
            ticketSecretF.      setText (Optional.ofNullable (ticket.getSecret ()).orElse (""));
            ticketVisitsF.      setText ("" + Optional.ofNullable (ticket.getVisits ()).orElse (0));
            
            final LocalDateTime dateTime = ticket.getLastTimeUsed ();
            ticketLastUsedF.setText (dateTime != null ? dateTime.toString () : "");
        }
        
        changedTicket.setClient (client.getId ());
    }
    
    private void bindFields () {
        idF.          textProperty  ().addListener ((__, ___, v) -> changedClient.setId (Integer.parseInt (v)));
        firstNameF.   textProperty  ().addListener ((__, ___, v) -> changedClient.setFirstName (v));
        secondNameF.  textProperty  ().addListener ((__, ___, v) -> changedClient.setSecondName (v));
        lastNameF.    textProperty  ().addListener ((__, ___, v) -> changedClient.setLastName (v));
        organizationF.textProperty  ().addListener ((__, ___, v) -> changedClient.setOrganization (v));
        positionF.    textProperty  ().addListener ((__, ___, v) -> changedClient.setPosition (v));
        countryF.     textProperty  ().addListener ((__, ___, v) -> changedClient.setCountry (v));
        stateF.       textProperty  ().addListener ((__, ___, v) -> changedClient.setState (v));
        cityF.        textProperty  ().addListener ((__, ___, v) -> changedClient.setCity (v));
        districtF.    textProperty  ().addListener ((__, ___, v) -> changedClient.setDistrict (v));
        phoneF.       textProperty  ().addListener ((__, ___, v) -> changedClient.setPhone (v));
        emailF.       textProperty  ().addListener ((__, ___, v) -> changedClient.setEmail (v));
        homePageF.    textProperty  ().addListener ((__, ___, v) -> changedClient.setHomePage (v));
        remarkF.      textProperty  ().addListener ((__, ___, v) -> changedClient.setRemark (v));
        birthdayF.    valueProperty ().addListener ((__, ___, v) -> changedClient.setBirthday (v));
        sexF.         valueProperty ().addListener ((__, ___, v) -> changedClient.setSex (v));
        
        ticketNameF.        textProperty ().addListener ((__, ___, v) -> changedTicket.setName (v));
        ticketSecretF.      textProperty ().addListener ((__, ___, v) -> changedTicket.setSecret (v));
        ticketLastUsedF.textProperty ().addListener ((__, ___, v) -> {
            LocalDateTime set = null;
            
            if (v.length () > 0) { set = LocalDateTime.parse (v); }
            changedTicket.setLastTimeUsed (set);
        });
        ticketVisitsF.      textProperty ().addListener ((__, ___, v) -> {
            Integer value = null;
            try   { value = Integer.parseInt (v); }
            catch (NumberFormatException nfe) {}
            changedTicket.setVisits (value);
        });
    }

    @Override
    public void close () throws Exception {
        
    }
    
}
