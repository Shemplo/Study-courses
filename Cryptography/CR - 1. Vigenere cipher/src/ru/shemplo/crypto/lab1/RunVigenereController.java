package ru.shemplo.crypto.lab1;

import java.io.IOException;

public class RunVigenereController {
    
    public static void main (String ... args) throws IOException {
        System.out.println (">> HW 1. Vigenere cipher <<");
        
        while (true) {
            System.out.print ("Select mode [(e)ncode, (d)ecode, (en)code-decode, (k)asiski, (s)top]: ");
            String mode = Utils.readInput ().toLowerCase ();
            if (mode.equals ("e")) {
                RunVigenereCipher.main ();
            } else if (mode.equals ("d")) {
                RunVigenereDecipher.main ();
            } else if (mode.equals ("en")) {
                System.out.print ("Message: ");
                String input = Utils.readInput ();
                System.out.print ("Key: ");
                String key = Utils.readInput ();
                
                String cipher = RunVigenereCipher.encrypt (input, key);
                System.out.println ();
                System.out.println (cipher);
                
                System.out.println ();
                System.out.println (RunVigenereDecipher.decrypt (cipher, key));
            } else if (mode.equals ("k")) {
                RunKasiskiTest.main ();
            } else if (mode.equals ("s")) {
                break;
            }
        }
    }
    
}
