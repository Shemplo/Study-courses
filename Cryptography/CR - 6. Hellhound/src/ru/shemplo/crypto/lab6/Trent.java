package ru.shemplo.crypto.lab6;

import static java.nio.charset.StandardCharsets.*;

import java.util.HashMap;
import java.util.Map;

public class Trent extends Endpoint {

    private final Map <String, byte []> user2key = new HashMap <> ();
    
    public Trent (int index, DeliveryService delivery) {
        super ("Trent #" + index, delivery);
    }
    
    public void shareKey (User user, byte [] key) {
        user2key.putIfAbsent (user.getName (), key);
    }

    @Override
    public void handleMessage (String from, Object [] message) {
        if ("initial".equals (message [0])) {
            final String a = (String) message [1], b = (String) message [2];
            System.out.println (String.format ("[%s] Starting session between: %s and %s", getName (), a, b));
            
            final byte [] aKey = user2key.get (a), bKey = user2key.get (b);
            
            final String timestamp = String.valueOf (System.currentTimeMillis ());
            final String ttl = String.valueOf (1000 * 10); // 10 seconds
            final String key = generateKey (32);
            
            final byte [] bCipher = cipherMessage (bKey, timestamp, ttl, key, a);
            final byte [] aCipher = cipherMessage (aKey, timestamp, ttl, key, new String (bCipher, ISO_8859_1), b);
            
            sendMessage (from, new Object [] {"session", aCipher});
        }
    }
    
}
