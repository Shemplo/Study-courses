package ru.shemplo.actor.aggregator.engine;

import java.time.Duration;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import akka.actor.AbstractActorWithTimers;
import akka.actor.ActorRef;
import ru.shemplo.actor.aggregator.engine.units.JSRequest;
import ru.shemplo.actor.aggregator.engine.units.JSResponse;
import ru.shemplo.snowball.utils.fp.AStream;

public class JAggregateEngine extends AbstractActorWithTimers implements AutoCloseable {
    
    private final class TimelimitIndicatior {}
    
    // $&*!@#* method `context.watch (reference)` doesn't add reference to children
    private final Set <ActorRef> children = new HashSet <> ();

    @Override
    public void preStart () throws Exception {
        AStream.make (Arrays.asList (JSActorDescriptor.values ()).stream ())
               .map (d -> d.apply (getContext ().getSystem ()))
               .forEach (children::add);
    }
    
    // The number of actors that can write response answer
    // Can be used as condition of early reply
    private int barrier = JSActorDescriptor.values ().length;
    private boolean responseSent = false;
    
    private ActorRef answerDestination;
    private JSResponse response;
    
    @Override
    public Receive createReceive () {
        return receiveBuilder ()
             . match (JSRequest.class, req -> {
                 // Saving reference where send the answer in future
                 this.answerDestination = getSender ();
                 
                 // Initializing empty response object (will be used for accumulation)
                 // TODO: return this object as Future and update it in resultList
                 this.response = JSResponse.empty (req);
                 
                 // Delegation request to children (they know what to do)
                 children.forEach (child -> {
                     child.tell (req, getSelf ());
                 });
                 
                 // Initializing timer that will indicate how much time actor can work
                 getTimers ().startSingleTimer ("TL", new TimelimitIndicatior (), 
                                                Duration.ofMillis (7500));
             })
             . match (JSResponse.class, resp -> {
                 // Accumulating response in single instance
                 response.mergeIn (resp);
                 barrier -= 1;
                 
                 if (barrier <= 0) { sendResponse (); }
             })
             . match (TimelimitIndicatior.class, 
                 __ -> sendResponse ())
             . matchAny (__ -> {
                 System.err.println ("Unknown message");
                 System.err.flush   ();
             })
             . build ();
    }
    
    private void sendResponse () {
        if (responseSent) { return; } // Response is already sent
        
        Long time = System.currentTimeMillis ();
        response.setJustFinished (true);
        response.setFinishTime (time);
        
        answerDestination.forward (response, getContext());
        responseSent = true;
        
        try { close (); } catch (Exception e) {}
    }
    
    @Override
    public void close () throws Exception {
        getContext ().getChildren ().forEach (this.getContext ()::stop);
        getContext ().stop (getSelf()); // Self-destruction :daemon:
    }
    
}
