package ru.shemplo.actor.aggregator.engine;

import java.net.URL;

import java.util.Arrays;

import akka.actor.AbstractActor;
import ru.shemplo.actor.aggregator.engine.units.JSRequest;
import ru.shemplo.actor.aggregator.engine.units.JSResponse;
import ru.shemplo.actor.aggregator.engine.units.JSResponse.JSResponseRow;
import scala.util.Random;

public class StubJSActor extends AbstractActor {
    
    @Override
    public Receive createReceive () {
        return receiveBuilder ()
             . match (JSRequest.class, req -> {
                 Thread.sleep (1000 + new Random ().nextInt (10000));
                 
                 Long time = System.currentTimeMillis ();
                 final JSActorDescriptor source = JSActorDescriptor.STUB_ACTOR;
                 getSender ().forward (new JSResponse (time, true, Arrays.asList (
                     new JSResponseRow (req.getQuery (), "Stub answer", 
                                        new URL ("http://localhost"), 
                                        source)
                 )), getContext ());
             })
             . build ();
    }
    
}
