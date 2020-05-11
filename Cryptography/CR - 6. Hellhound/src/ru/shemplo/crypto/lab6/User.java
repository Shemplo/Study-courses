package ru.shemplo.crypto.lab6;

import static java.nio.charset.StandardCharsets.*;

import java.util.HashMap;
import java.util.Map;

public class User extends Endpoint {
    
    private final Map <String, Long> handshakeValues = new HashMap <> ();
    private final Map <String, byte []> sessionKeys = new HashMap <> ();
    
    private final Map <String, Long> sessionDuration = new HashMap <> ();
    private final Map <String, Long> sessionStart = new HashMap <> ();
    
    private final byte [] key;
    
    public User (String name, DeliveryService delivery) {
        super (name, delivery);
        
        final var strKey = generateKey (32);
        key = strKey.getBytes ();
        
        delivery.getDirectChannelWithTrent (this, "Trent #0").ifPresent (host -> {
            host.shareKey (this, key);
        });
    }
    
    @Override
    public void handleMessage (String from, Object [] message) {
        if ("session".equals (message [0])) {
            final var bundle = parseMessage ((byte []) message [1], key);
            
            final long timestamp = Long.parseLong (bundle [0]);
            final long duration = Long.parseLong (bundle [1]);
            
            final byte [] keyAB = bundle [2].getBytes ();
            final String partner = bundle [4].trim ();
            sessionKeys.put (partner, keyAB);
            
            sessionDuration.put (partner, duration);
            sessionStart.put (partner, timestamp);
            
            final long now = System.currentTimeMillis ();
            handshakeValues.put (partner, now + 1);
            
            final byte [] handshadeCipher = cipherMessage (keyAB, String.valueOf (now), getName ());
            
            sendMessage (partner, new Object [] {"join", handshadeCipher, bundle [3]});
        } else if ("join".equals (message [0])) {
            final byte [] cipher = ((String) message [2]).getBytes (ISO_8859_1);
            final var bundle = parseMessage (cipher, key);
            
            final long duration = Long.parseLong (bundle [1]);
            final long start = Long.parseLong (bundle [0]);
            
            final String partner = bundle [3].trim ();
            final byte [] keyAB = bundle [2].getBytes ();
            sessionKeys.put (partner, keyAB);
            
            sessionDuration.put (partner, duration);
            sessionStart.put (partner, start);
            
            final String [] handshakeBundle = parseMessage ((byte []) message [1], keyAB);
            
            if (partner.equals (handshakeBundle [1].trim ())) {
                final long timestamp = Long.parseLong (handshakeBundle [0]) + 1;
                
                final var response = cipherMessage (keyAB, String.valueOf (timestamp));
                sendMessage (partner, new Object [] {"handshake", response});
            }
        } else if ("handshake".equals (message [0])) {
            if (sessionKeys.containsKey (from)) {
                final var keyAB = sessionKeys.get (from);
                final var bundle = parseMessage ((byte []) message [1], keyAB);
                
                final long timestamp = Long.parseLong (bundle [0].trim ());
                if (timestamp == handshakeValues.get (from)) {
                    System.out.println ("+++ Connection with " + from + " established +++");
                    handshakeValues.remove (from);
                    System.out.println ("\n\n");
                    
                    final var cipher = cipherMessage (keyAB, "My name is Alice");
                    sendMessage ("Bob", new Object [] {"chat", cipher});
                }
            }
        } else if ("chat".equals (message [0])) {
            final var keyAB = sessionKeys.get (from);
            final var bundle = parseMessage ((byte []) message [1], keyAB);
            
            final var text = bundle [0].trim ();
            System.out.format ("[%s] Chat message from '%s': %s\n", getName (), from, text);
            final var expirationTime = sessionStart.get (from) + sessionDuration.get (from);
            
            if (System.currentTimeMillis () >= expirationTime) {
                System.out.format ("--- Session with '%s' is expired ---\n", from);
            } else {
                String response = text.length () > 30 
                     ? text.substring (text.length () - 30) 
                     : text;
                response += "-" + getName ();
                
                try {
                    Thread.sleep (1000 + random.nextInt (500));
                } catch (InterruptedException e) {}
                
                final var cipher = cipherMessage (keyAB, response);
                sendMessage (from, new Object [] {"chat", cipher});
            }
        }
    }
    
}
