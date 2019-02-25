package ru.shemplo.actor.aggregator;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import java.util.HashMap;
import java.util.Map;

import javafx.application.Application;

import org.yaml.snakeyaml.Yaml;

import akka.actor.ActorSystem;
import lombok.Getter;
import ru.shemplo.actor.aggregator.gui.AggregatorWindow;
import ru.shemplo.snowball.stuctures.Pair;

public class RunSearchAggregator {
    
    @Getter private static Map <String, Object> configuration;
    
    @Getter private static final ActorSystem actors 
        = ActorSystem.create ("aggregator");
    
    
    public static void main (String ... args) throws Exception {
        final File file = new File ("search-configuration.yml");
        InputStream is = new FileInputStream (file);
        configuration = new Yaml ().load (is);
        
        // Making from {a={b=foo,c=bar}} -> {a.b=foo, a.c=bar}
        configuration = flatConfigurationKeys (configuration);
        
        Application.launch (AggregatorWindow.class, args);
    }
    
    private static Map <String, Object> flatConfigurationKeys (Map <String, Object> config) {
        Map <String, Object> configuration = new HashMap <> ();
        config.entrySet ().stream ().map (Pair::fromMapEntry).forEach (p -> {
            if (p.getS () instanceof Map) {
                @SuppressWarnings ("unchecked")
                Map <String, Object> tmp = (Map <String, Object>) p.S;
                flatConfigurationKeys (tmp).forEach ((k, v) -> {
                    configuration.put (String.join (".", p.F, k), v);
                });
            } else {
                configuration.put (p.F, p.S);
            }
        });
        
        return configuration;
    }
    
}
