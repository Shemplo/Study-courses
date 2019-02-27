package ru.shemplo.actor.aggregator.engine;

import static akka.actor.Props.*;
import static ru.shemplo.actor.aggregator.RunSearchAggregator.*;

import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public enum SActorDescriptor implements Function <ActorSystem, ActorRef> {
    
    YANDEX_ACTOR ("Yandex",  YandexSActor.class, conf -> conf.containsKey ("yandex.key")),
    GOOGLE_ACTOR ("Google",  GoogleSActor.class, conf -> conf.containsKey ("google.key")),
    YAHOO_ACTOR  ("Yahoo",   YahooSActor.class,  __ -> true),
    BING_ACTOR   ("Bing",    BingSActor.class,   __ -> true),
    STUB_ACTOR   ("My Stub", StubSActor.class,   __ -> true)
    ;
    
    @Getter private final String title;
    
    private final Class <? extends AbstractActor>  token;
    private final Predicate <Map <String, Object>> activate;

    @Override
    public ActorRef apply (ActorSystem system) {
        Map <String, Object> config = getConfiguration ();
        
        if (activate.test (config)) {
            return system.actorOf (create (token));
        }
        
        return null;
    }
    
}
