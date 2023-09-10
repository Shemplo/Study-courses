package ru.shemplo.crypto.lab7;

import static java.math.BigInteger.*;

import java.math.BigInteger;
import java.util.Random;

import ru.shemplo.crypto.lab5.RunRSACipher;
import ru.shemplo.crypto.lab5.RunRSADecipher;

public class DHController {
    
    public static final Random r = new Random ();
    
    public static final BigInteger G = probablePrime (7, r),
                                   P = probablePrime (50, r);
    
    public static void main (String ... args) {
        /* Welcome to Unicode reality
        System.out.println ();
        System.out.println ();
        
        System.out.println ((char) 96933);
        System.out.println ((char) 31397);
        
        System.out.println (new String (new char [] {(char) 96933}).equals (new String (new char [] {(char) 31397})));
        System.out.println ((int) new String (new char [] {(char) 96933}).toCharArray () [0]);
        System.out.println ();
        System.out.println ();
        */
        
        System.out.println (">>> Generation of public numbers P and G <<<");
        final var pm1d2 = P.subtract (ONE).divide (TWO);
        System.out.println ("Is `(p - 1) / 2` primary number: " + pm1d2.isProbablePrime (10));
        System.out.println ("P = " + P + "; G = " + G);
        System.out.println ();
        
        System.out.println (">>> Generation of private keys of Alice (a) and Bob (b)  <<<");
        final var a = valueOf (Math.abs (r.nextLong ()));
        final var b = valueOf (Math.abs (r.nextLong ()));
        System.out.println ("a = " + a + "; b = " + b);
        System.out.println ();
        
        System.out.println (">>> Evaluation of G^a and G^b for Alice (A) and Bob (B) <<<");
        final var A = G.modPow (a, P);
        final var B = G.modPow (b, P);
        System.out.println ("A = " + A + "; B = " + B);
        System.out.println ();
        
        System.out.println (">>> Generation of RSA key-pair for Alice <<<");
        final var EDNa = RunRSACipher.encrypt ("".toCharArray ());
        System.out.println ("E, D, N for Alice: " + EDNa);
        final var Ca = RunRSACipher.encrypt (A.toString ().toCharArray (), EDNa.T3, EDNa.T4);
        //System.out.println ("Signed with Da A value: " + new String (Ca, 0, Ca.length));
        System.out.println ();
        
        System.out.println (">>> Generation of RSA key-pair for Bob <<<");
        final var EDNb = RunRSACipher.encrypt ("".toCharArray ());
        System.out.println ("E, D, N for Bob: " +  EDNb);
        final var Cb = RunRSACipher.encrypt (B.toString ().toCharArray (), EDNb.T3, EDNb.T4);
        //System.out.println ("Signed with Db B value: " + new String (Cb, 0, Cb.length));
        System.out.println ();
        
        System.out.println (">>> Deciphering signed B and A values for Alice (Ra) and Bob (Rb) <<<");
        final var Ra = new BigInteger (new String (RunRSADecipher.decrypt (Cb, EDNb.T2, EDNb.T4)));
        final var Rb = new BigInteger (new String (RunRSADecipher.decrypt (Ca, EDNa.T2, EDNa.T4)));
        System.out.println ("Ra = " + Ra + "; Rb = " + Rb);
        System.out.println ();
        
        System.out.println (">>> Evaluating common private key for Alice (Ka) and Bob (Kb) <<<");
        final var Ka = Ra.modPow (a, P); // Alice does
        final var Kb = Rb.modPow (b, P); // Bob does
        System.out.println ("Ka = " + Ka + "; Kb = " + Kb + "; is equal = " + Ka.equals (Kb));
        System.out.println ();
        
        final var K = Ka.max (Kb);
        
        System.out.println ("*** Secret key: " + K + " ***");
    }
    
}
