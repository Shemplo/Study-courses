package ru.shemplo.actor.aggregator;

import static ru.shemplo.snowball.utils.fp.FunctionalUtils.*;
import static ru.shemplo.snowball.utils.fp.FunctionalUtils.Case.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.pattern.Patterns;
import ru.shemplo.actor.aggregator.engine.AggregateEngine;
import ru.shemplo.actor.aggregator.engine.SearchRequest;
import ru.shemplo.snowball.utils.StringManip;
import scala.util.Failure;
import scala.util.Success;

public class RunActorAggregator {
    
    private static final ActorSystem aggregator = ActorSystem.create ("aggregator");
    
    private static final Props ENGINE_PROPS = Props.create (AggregateEngine.class);
    private static final ActorRef engine = aggregator.actorOf (ENGINE_PROPS,"engine");
    
    public static void main (String ... args) throws Exception {
        try (
            Reader r = new InputStreamReader (System.in,
                                StandardCharsets.UTF_8);
            BufferedReader br = new BufferedReader (r);
        ) {
            String line = null;
            while ((line = StringManip.fetchNonEmptyLine (br)) != null) {
                if ((Boolean) switch$ (line.trim ().toLowerCase (),
                        caseOf (s -> s.matches ("^exit$"),
                            __ -> true),
                        caseOf (s -> s.matches ("^search .+$"),
                            RunActorAggregator::sendSearchRequest),
                        caseOf (__ -> true,
                            v -> {
                                System.out.println (v);
                                return false;
                            })
                )) { break; }
            }
        }
        
        System.out.println ("End");
        aggregator.terminate ();
    }
    
    private static boolean sendSearchRequest (String query) {
        Patterns.ask (engine, new SearchRequest (query), 1000)
                .onComplete (r -> switch$ (r,
                    caseOf (t -> t instanceof Success, t -> {
                        System.out.println ("Success: " + t);
                        return t;
                    }),
                    caseOf (t -> t instanceof Failure, t -> {
                        System.out.println ("Fail: " + t);
                        return t;
                    })), aggregator.dispatcher ());
        return false; // to continue execution of application
    }
    
}
