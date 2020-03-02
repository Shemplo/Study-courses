package ru.shemplo.crypto.lab2;

import java.io.IOException;

public class RunDESController {
    
    public static void main (String ... args) throws IOException {
        System.out.println (">> HW 2. DES cipher <<");
        
        while (true) {
            System.out.print ("Select mode [(e)ncrypt, (d)ecrypt, (en)crypt-decrypt, (s)top]: ");
            String mode = Utils.readInput ().toLowerCase ();
            if (mode.equals ("e")) {
                RunDESCipher.main ();
            } else if (mode.equals ("d")) {
                RunDESCipher.main ();
            } else if (mode.equals ("en")) {
                System.out.print ("Message: ");
                String input = Utils.readInput ();
                System.out.print ("Key: ");
                String key = Utils.readInput ();
                
                byte [] cipher = RunDESCipher.encrypt (input.getBytes (), key.getBytes ());
                System.out.println ();
                System.out.println (new String (cipher));
                
                byte [] message = RunDESDecipher.decrypt (cipher, key.getBytes ());
                System.out.println ();
                System.out.println (new String (message));
            } else if (mode.equals ("s")) {
                break;
            }
        }
    }
    
}
