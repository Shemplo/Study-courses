package ru.shemplo.fitness.administration;

import java.io.IOException;
import java.nio.file.Paths;
import java.sql.SQLException;

import javafx.application.Application;

import ru.shemplo.fitness.AppConfiguration;
import ru.shemplo.fitness.administration.gfx.AdminApplication;
import ru.shemplo.fitness.db.DBManager;
import ru.shemplo.fitness.entities.SeasonTicket;
import ru.shemplo.fitness.services.SeasonTicketService;
import ru.shemplo.snowball.annot.Wind;
import ru.shemplo.snowball.annot.processor.Snowball;

@Wind (blow = {AppConfiguration.class, SeasonTicketService.class,
               DBManager.class})
public class RunFitnessAdministration extends Snowball {
    
    public static void main (String ... args) { shape (args); }
    
    private AppConfiguration configuration;
    private DBManager db;
    
    @Override
    protected void onShaped (String ... args) {
        try {
            configuration.readConfigurationFile (Paths.get ("config.yml"));
        } catch (IOException ioe) { ioe.printStackTrace (); }
        
        System.out.println (configuration.get ("database-profile"));
        try {
            db.retrieve ("SELECT * FROM `season_tickets`", SeasonTicket.class).forEach (System.out::println);
        } catch (IOException | SQLException e) {
            e.printStackTrace();
        }
        
        Application.launch (AdminApplication.class, args);
    }
    
}
