package ru.shemplo.crypto.lab6;

import java.security.SecureRandom;
import java.util.Random;
import java.util.stream.IntStream;

import ru.shemplo.crypto.lab3.TwofishCipher;

public abstract class Endpoint {
    
    protected final Random random = new SecureRandom ();
    
    protected final String name;
    
    private final DeliveryService delivery;
    
    public Endpoint (String name, DeliveryService delivery) {
        this.name = name; this.delivery = delivery;
    }
    
    public String getName () {
        return name;
    }
    
    protected String generateKey (int length) {
        final var codes = IntStream.range (0, length).map (__ -> random.nextInt (Character.MAX_VALUE)).toArray ();
        return new String (codes, 0, codes.length);
    }
    
    protected String [] parseMessage (byte [] cipher, byte [] key) {
        return new String (TwofishCipher.decrypt (cipher, key)).split (">>>");
    }
    
    protected byte [] cipherMessage (byte [] key, String ... values) {
        final String bundle = String.join (">>>", values);
        return TwofishCipher.encrypt (bundle.getBytes (), key);
    }
    
    protected void sendMessage (String endpoint, Object [] message) {
        delivery.deliver (getName (), endpoint, message);
    }
    
    public abstract void handleMessage (String from, Object [] message);
    
}
