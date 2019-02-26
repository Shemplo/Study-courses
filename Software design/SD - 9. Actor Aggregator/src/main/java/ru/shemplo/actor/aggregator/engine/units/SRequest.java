package ru.shemplo.actor.aggregator.engine.units;

import lombok.Data;

@Data
public class SRequest {

    private SResponse response;

    private final String query;
    
}
