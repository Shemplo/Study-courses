package ru.shemplo.actor.aggregator.engine;

import java.net.URL;

import java.util.List;
import java.util.Map;

import com.mashape.unirest.http.Unirest;
import com.mashape.unirest.http.exceptions.UnirestException;

import akka.actor.AbstractActor;
import ru.shemplo.actor.aggregator.RunSearchAggregator;
import ru.shemplo.actor.aggregator.engine.units.SRequest;
import ru.shemplo.actor.aggregator.engine.units.SResponse;
import ru.shemplo.actor.aggregator.engine.units.SResponse.SResponseRow;

public abstract class AbsSActor extends AbstractActor {
    
    protected final Map <String, Object> configuration = 
        RunSearchAggregator.getConfiguration ();
    
    @Override
    public Receive createReceive () {
        return receiveBuilder ()
             . match (SRequest.class, this::sendRequest)
             . build ();
    }
    
    protected abstract URL makeGetRequestURL (SRequest request);
    
    protected abstract List <SResponseRow> parseResponse (String response);
    
    protected void sendRequest (SRequest request) {
        URL requestLink = makeGetRequestURL (request);
        try {
            String body = Unirest.get (requestLink.toString ())
                                 .asString ().getBody ();
            List <SResponseRow> rows = parseResponse (body);
            final Long time = System.currentTimeMillis ();
            
            SResponse response = new SResponse (time, true, rows);
            getSender ().forward (response, getContext ());
        } catch (UnirestException ue) {
            ue.printStackTrace ();
        }
    }
    
}
