package ru.shemplo.crypto.lab6;


public class RunWorld {
    
    public static void main (String ... args) {
        final var delivery = new DeliveryService ();
        
        final var zinger = new Trent (0, delivery);
        delivery.addEndpoint (zinger);
        
        final var alice = new User ("Alice", delivery) {
            
            public void initiateDialog (String endpoint) {
                sendMessage ("Trent #0", new String [] {"initial", "Alice", "Bob"});
            }
            
        };
        delivery.addEndpoint (alice);
        
        final var bob = new User ("Bob", delivery);
        delivery.addEndpoint (bob);
        
        alice.initiateDialog (bob.getName ());
    }
    
}
