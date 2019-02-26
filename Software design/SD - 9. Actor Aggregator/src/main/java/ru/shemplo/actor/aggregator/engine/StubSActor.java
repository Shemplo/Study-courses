package ru.shemplo.actor.aggregator.engine;

import java.net.URL;

import java.util.Arrays;

import akka.actor.AbstractActor;
import ru.shemplo.actor.aggregator.engine.units.SRequest;
import ru.shemplo.actor.aggregator.engine.units.SResponse;
import ru.shemplo.actor.aggregator.engine.units.SResponse.SResponseRow;
import scala.util.Random;

public class StubSActor extends AbstractActor {
    
    @Override
    public Receive createReceive () {
        return receiveBuilder ()
             . match (SRequest.class, req -> {
                 Thread.sleep (100 + new Random ().nextInt (2000));
                 
                 Long time = System.currentTimeMillis ();
                 final SActorDescriptor source = SActorDescriptor.STUB_ACTOR;
                 getSender ().forward (new SResponse (time, true, Arrays.asList (
                     new SResponseRow (req.getQuery (), "Stub answer", 
                                        new URL ("http://localhost"), 
                                        source)
                 )), getContext ());
             })
             . build ();
    }
    
}
