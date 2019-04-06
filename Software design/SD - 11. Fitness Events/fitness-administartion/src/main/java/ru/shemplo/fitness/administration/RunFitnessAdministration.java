package ru.shemplo.fitness.administration;

import java.io.IOException;
import java.nio.file.Paths;

import javafx.application.Application;

import ru.shemplo.fitness.AppConfiguration;
import ru.shemplo.fitness.administration.gfx.AdminApplication;
import ru.shemplo.fitness.db.DBManager;
import ru.shemplo.fitness.services.SeasonTicketService;
import ru.shemplo.fitness.statistics.StatisticsModule;
import ru.shemplo.snowball.annot.Wind;
import ru.shemplo.snowball.annot.processor.Snowball;

@Wind (blow = {AppConfiguration.class, SeasonTicketService.class,
               DBManager.class, StatisticsModule.class})
public class RunFitnessAdministration extends Snowball {
    
    public static void main (String ... args) { shape (args); }
    
    private AppConfiguration configuration;
    
    @Override
    protected void onShaped (String ... args) {
        try   { configuration.readConfigurationFile (Paths.get ("config.yml")); } 
        catch (IOException ioe) { 
            ioe.printStackTrace (); 
            System.exit (1);
        }
        
        Application.launch (AdminApplication.class, args);
    }
    
}
