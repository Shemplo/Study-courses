package ru.shemplo.crypto.lab3;

import java.io.IOException;

public class RunTwofishController {
    
    public static void main (String ... args) throws IOException {
        System.out.println (">> HW 3. Twofish cipher <<");
        
        while (true) {
            System.out.print ("Select mode [(en)crypt-decrypt, (s)top]: ");
            String mode = Utils.readInput ().toLowerCase ();
            if (mode.equals ("en")) {
                System.out.print ("Message: ");
                String input = Utils.readInput ();
                System.out.print ("Key: ");
                String key = Utils.readInput ();
                
                byte [] cipher = TwofishCipher.encrypt (input.getBytes (), key.getBytes ());
                System.out.println ();
                System.out.println (new String (cipher));
                
                byte [] message = TwofishCipher.decrypt (cipher, key.getBytes ());
                System.out.println ();
                System.out.println (new String (message));
            } else if (mode.equals ("s")) {
                break;
            }
        }
    }
    
}
