package ru.shemplo.actor.aggregator.engine;

import static akka.actor.Props.*;

import java.util.function.Function;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public enum JSActorDescriptor implements Function <ActorSystem, ActorRef> {
    
    YANDEX_ACTOR (YandexJSActor.class),
    GOOGLE_ACTOR (GoogleJSActor.class),
    STUB_ACTOR   (StubJSActor.class)
    ;
    
    private final Class <? extends AbstractActor> token;

    @Override
    public ActorRef apply (ActorSystem system) {
        return system.actorOf (create (token));
    }
    
}
