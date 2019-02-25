package ru.shemplo.actor.aggregator.engine;

import java.net.URL;

import java.util.List;
import java.util.Map;

import com.mashape.unirest.http.Unirest;
import com.mashape.unirest.http.exceptions.UnirestException;

import akka.actor.AbstractActor;
import ru.shemplo.actor.aggregator.RunSearchAggregator;
import ru.shemplo.actor.aggregator.engine.units.JSRequest;
import ru.shemplo.actor.aggregator.engine.units.JSResponse;
import ru.shemplo.actor.aggregator.engine.units.JSResponse.JSResponseRow;

public abstract class AbsJSActor extends AbstractActor {
    
    protected final Map <String, Object> configuration = 
        RunSearchAggregator.getConfiguration ();
    
    @Override
    public Receive createReceive () {
        return receiveBuilder ()
             . match (JSRequest.class, this::sendRequest)
             . build ();
    }
    
    protected abstract URL makeGetRequestURL (JSRequest request);
    
    protected abstract List <JSResponseRow> parseResponse (String response);
    
    protected void sendRequest (JSRequest request) {
        URL requestLink = makeGetRequestURL (request);
        try {
            String body = Unirest.get (requestLink.toString ())
                                 .asString ().getBody ();
            List <JSResponseRow> rows = parseResponse (body);
            final Long time = System.currentTimeMillis ();
            
            JSResponse response = new JSResponse (time, true, rows);
            getSender ().forward (response, getContext ());
        } catch (UnirestException ue) {
            ue.printStackTrace ();
        }
    }
    
}
