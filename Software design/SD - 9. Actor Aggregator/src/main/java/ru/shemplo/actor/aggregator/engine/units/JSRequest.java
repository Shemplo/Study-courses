package ru.shemplo.actor.aggregator.engine.units;

import lombok.Data;

@Data
public class JSRequest {

    private JSResponse response;

    private final String query;
    
}
