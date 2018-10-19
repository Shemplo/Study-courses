package ru.shemplo.tasks;

import static ru.shemplo.tasks.PropertiesLoader.*;

import java.io.IOException;
import java.io.InputStream;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Import;

import ru.shemplo.tasks.conf.WebAppConfig;

@Import (WebAppConfig.class)
@SpringBootApplication
public class Run {

    static {
        try   { load ("application.properties"); } 
        catch (IOException ioe) {
            InputStream is = Run.class.getResourceAsStream ("/application.properties");
            if (is != null) {
                try   { load (is); } 
                catch (IOException ioe2) {
                    System.err.println ("Failed to load properties file");
                    ioe2.printStackTrace ();
                    System.exit (1);
                } finally {
                    try { is.close (); }
                    catch (IOException ioe2) {}
                }
            } else {
                System.err.println ("Failed to load properties file");
                System.exit (1);
            }
        }
    }
    
    public static void main (String ... args) {
        SpringApplication.run (Run.class, args);
    }
    
}
