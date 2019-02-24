package ru.shemplo.actor.aggregator;

import javafx.application.Application;

import akka.actor.ActorSystem;
import lombok.Getter;
import ru.shemplo.actor.aggregator.gui.AggregatorWindow;

public class RunSearchAggregator {
    
    @Getter private static final ActorSystem actors 
        = ActorSystem.create ("aggregator");
    
    public static void main (String ... args) throws Exception {
        Application.launch (AggregatorWindow.class, args);
    }
    
}
