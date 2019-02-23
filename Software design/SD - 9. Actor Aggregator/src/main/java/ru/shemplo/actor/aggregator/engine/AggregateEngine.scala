package ru.shemplo.actor.aggregator.engine

import akka.actor.Actor

class AggregateEngine extends Actor {
    
    override def receive: Receive = {
        case request : SearchRequest => sender().forward ("answer: " + request.query)
        case str : String => sender().forward("answer: " + str)
        case _ => System.out.println ("Message of unknown type")
    }
    
}
