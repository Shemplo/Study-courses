package ru.shemplo.crypto.lab2;

import static ru.shemplo.crypto.lab2.Permutations.*;
import static ru.shemplo.crypto.lab2.Utils.*;

import java.io.IOException;

public class RunDESDecipher {
    
    private static final byte [] rotations = { 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1 };
    
    public static void main (String ... args) throws IOException {
        System.out.print ("Encoded message: ");
        String input = Utils.readInput ();
        System.out.print ("Key: ");
        String key = Utils.readInput ();
        
        byte [] message = decrypt (input.getBytes (), key.getBytes ());
        System.out.println ();
        System.out.println (new String (message));
    }
    
    public static byte [] decrypt (byte [] cipher, byte [] key) {
        byte [] message = new byte [cipher.length];
        
        long lkey = getLongFromBytes (key, 0);
        for (int i = 0; i < cipher.length; i += 8) {
            decrypt (cipher, i, message, i, lkey);
        }
        
        return message;
    }
    
    private static void decrypt (
        byte [] cipher, int ciphertextOffset, 
        byte [] message, int messageOffset,
        long key
    ) {
        long c = getLongFromBytes (cipher, ciphertextOffset);
        long m = decrypt (c, key);
        getBytesFromLong (message, messageOffset, m);
    }
    
    private static long decrypt (long c, long key) {
        long [] subkeys = createSubkeys (key, rotations);
        long ip = permuteI (c);
        
        int l = (int) (ip >> 32);
        int r = (int) (ip & 0xFFFFFFFFL);
        
        for (int i = 15; i > -1; i--) {
            int previous_l = l;
            
            l = r;
            r = previous_l ^ feistel (r, subkeys [i]);
        }
        
        long rl = (r & 0xFFFFFFFFL) << 32 | (l & 0xFFFFFFFFL);
        long fp = permuteF (rl);
        
        return fp;
    }
    
}
