package ru.shemplo.fitness.app.services.responses;

import java.io.IOException;

public class ResponseTicketVisits extends AbsResponse <Integer, IOException> {

    public ResponseTicketVisits (Integer value, IOException exception) {
        super (value, exception);
    }

}
