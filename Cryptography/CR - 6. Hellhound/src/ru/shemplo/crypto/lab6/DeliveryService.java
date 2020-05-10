package ru.shemplo.crypto.lab6;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class DeliveryService {
    
    private final Map <String, Endpoint> name2endpoint = new HashMap <> ();
    
    public void addEndpoint (Endpoint endpoint) {
        name2endpoint.putIfAbsent (endpoint.getName (), endpoint);
    }
    
    public Optional <Trent> getDirectChannelWithTrent (User user, String trentName) {
        final var endpoint = name2endpoint.get (trentName);
        
        if (endpoint != null && endpoint instanceof Trent) {
            return Optional.of ((Trent) endpoint);
        }
        
        return Optional.empty ();
    }
    
    public void deliver (String from, String to, Object [] message) {
        name2endpoint.get (to).handleMessage (from, message);
    }
    
}
