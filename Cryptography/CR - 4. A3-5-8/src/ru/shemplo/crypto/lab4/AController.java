package ru.shemplo.crypto.lab4;

import java.io.IOException;
import java.util.Arrays;

public class AController {
    
    public static void main (String ... args) throws IOException {
        System.out.println (">> HW 4. A3, A5, A8<<");
        final var cipherA38 = new COMP128Cipher ();
        final var cipherA5 = new A5Cipher ();
        
        while (true) {
            System.out.print ("Select mode [(A)3/8, (e)ncrypt-decrypt A5, (s)top]: ");
            String mode = Utils.readInput ().toLowerCase ();
            if (mode.equals ("a")) {
                System.out.println ("Template: 00000000000000000000000000000000");
                System.out.print ("Key (32 hex digits): 0x");
                String key = Utils.readInput ();
                System.out.print ("Random (32 hex digits): 0x");
                String random = Utils.readInput ();
                
                int [] key_  = Utils.compressInput (key), 
                       rand_ = Utils.compressInput (random);
                int [] out = cipherA38.encrypt (rand_, key_);
                System.out.print ("Out: ");
                for (int i = 0; i < out.length; i++) {
                    System.out.print (String.format ("%02x ", out [i]));
                }
                System.out.println ();
            } else if (mode.equals ("e")) {
                System.out.print ("Message: ");
                String input = Utils.readInput ();
                
                final var cipher = cipherA5.encrypt (input.toCharArray ());
                System.out.println ();
                System.out.println ("Cipher: " + new String (cipher.T1));
                System.out.println ("Secret: " + Arrays.toString (cipher.T2));
                
                char [] message = cipherA5.decrypt (cipher.T1, cipher.T2);
                System.out.println ();
                System.out.println (new String (message));
            } else if (mode.equals ("s")) {
                break;
            }
        }
    }
    
}
