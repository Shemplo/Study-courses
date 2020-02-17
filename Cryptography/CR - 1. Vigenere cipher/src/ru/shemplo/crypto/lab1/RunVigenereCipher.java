package ru.shemplo.crypto.lab1;

import java.io.IOException;

public class RunVigenereCipher {
    
    public static void main (String ... args) throws IOException {
        System.out.print ("Message: ");
        String input = Utils.readInput ();
        System.out.print ("Key: ");
        String key = Utils.readInput ();
        
        System.out.println ();
        System.out.println (encode (input, key));
    }
    
    private static String encode (String input, String key) {
        char [] buffer = new char [input.length ()];
        
        for (int i = 0; i < input.length (); i++) {
            if (!Character.isLetter (input.charAt (i))) {
                buffer [i] = input.charAt (i);
                continue;
            }
            
            char c = (char) (input.charAt (i) - 'a');
            char k = (char) (key.charAt (i % key.length ()) - 'a');
            
            buffer [i] = (char) ('a' + (c + k) % 26);
        }
        
        return String.valueOf (buffer);
    }
    
}
