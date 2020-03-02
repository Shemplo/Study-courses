package ru.shemplo.crypto.lab1;

import java.io.IOException;

public class RunVigenereDecipher {
    
    public static void main (String ... args) throws IOException {
        System.out.print ("Encoded message: ");
        String input = Utils.readInput ();
        System.out.print ("Key: ");
        String key = Utils.readInput ();
        
        System.out.println ();
        System.out.println (decrypt (input, key));
    }
    
    public static String decrypt (String input, String key) {
        char [] buffer = new char [input.length ()];
        
        for (int i = 0; i < input.length (); i++) {
            /*
            if (!Character.isLetter (input.charAt (i))) {
                buffer [i] = input.charAt (i);
                continue;
            }
            
            char c = (char) (input.charAt (i) - 'a');
            char k = (char) (key.charAt (i % key.length ()) - 'a');
            
            buffer [i] = (char) ('a' + (c - k + 26) % 26);
            */
            
            final char k = key.charAt (i % key.length ());
            buffer [i] = (char) (input.charAt (i) - k);
        }
        
        return String.valueOf (buffer);
    }
    
}
