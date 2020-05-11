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
        System.out.format ("[Delivery Service] User '%s' requested direct access to '%s'\n", user.getName (), trentName);
        final var endpoint = name2endpoint.get (trentName);
        
        if (endpoint != null && endpoint instanceof Trent) {
            return Optional.of ((Trent) endpoint);
        }
        
        return Optional.empty ();
    }
    
    public void deliver (String from, String to, Object [] message) {
        System.out.format ("[Delivery Service] Endpoint '%s' sent message to '%s':\n", from, to);
        for (int i = 0; i < message.length; i++) {
            if (message [i] instanceof String) {
                String text = (String) message [i];
                System.out.format ("    %d. Text message: %s\n", i, text);
            } else if (message [i] instanceof byte []) {
                byte [] cipher = (byte []) message [i];
                final var text = new String (cipher).replace ("\n", "\\n").replace ("\r", "\\r");
                System.out.format ("    %d. Cipher: %s\n", i, text);
            }
        }
        System.out.println ();
        
        name2endpoint.get (to).handleMessage (from, message);
    }
    
}
