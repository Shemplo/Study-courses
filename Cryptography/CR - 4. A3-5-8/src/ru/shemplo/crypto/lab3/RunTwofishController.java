package ru.shemplo.crypto.lab3;

import java.io.IOException;

public class RunTwofishController {
    
    public static void main (String ... args) throws IOException {
        System.out.println (">> HW 4. A3, A5, A8<<");
        final var cipherA38 = new COMP128Cipher ();
        
        while (true) {
            System.out.print ("Select mode [(A)3/8, (en)crypt-decrypt A5, (s)top]: ");
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
            } else if (mode.equals ("en")) {
                //System.out.print ("Message: ");
                //String input = Utils.readInput ();
                //System.out.print ("Key: ");
                //String key = Utils.readInput ();
                
                //byte [] cipher = TwofishCipher.encrypt (input.getBytes (), key.getBytes ());
                //System.out.println ();
                //System.out.println (new String (cipher));
                
                //byte [] message = TwofishCipher.decrypt (cipher, key.getBytes ());
                //System.out.println ();
                //System.out.println (new String (message));
            } else if (mode.equals ("s")) {
                break;
            }
        }
    }
    
}
