package ru.shemplo.actor.aggregator.engine;

import java.net.URL;

import java.util.Arrays;

import akka.actor.AbstractActor;
import ru.shemplo.actor.aggregator.engine.units.JSRequest;
import ru.shemplo.actor.aggregator.engine.units.JSResponse;
import ru.shemplo.actor.aggregator.engine.units.JSResponse.JSResponseRow;

public class YandexJSActor extends AbstractActor {

    @Override
    public Receive createReceive () {
        return receiveBuilder ()
             . match (JSRequest.class, req -> {
                 Thread.sleep (2000);
                 
                 Long time = System.currentTimeMillis ();
                 getSender ().forward (new JSResponse (req, time, true, Arrays.asList (
                     new JSResponseRow (req.getQuery (), "Answer from yandex", new URL ("https://yandex.ru/"))
                 )), getContext ());
             })
             . build ();
    }
    
}
