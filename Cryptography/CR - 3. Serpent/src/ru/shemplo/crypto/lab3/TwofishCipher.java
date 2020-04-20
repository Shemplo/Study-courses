package ru.shemplo.crypto.lab3;

import static ru.shemplo.crypto.lab3.Matrices.*;
import static ru.shemplo.crypto.lab3.Utils.*;

import java.util.Arrays;

public class TwofishCipher {
    
    private static final int BLOCK_SIZE = 16;
    private static final int ROUNDS = 16;
    private static final int INPUT_WHITEN = 0;
    private static final int OUTPUT_WHITEN = INPUT_WHITEN + BLOCK_SIZE / 4;
    private static final int ROUND_SUBKEYS = OUTPUT_WHITEN + BLOCK_SIZE / 4;
    private static final int SK_STEP = 0x02020202;
    private static final int SK_BUMP = 0x01010101;
    private static final int SK_ROTL = 9;
    
    public static byte [] encrypt (byte [] message, byte [] key) {
        message = Arrays.copyOf (message, 128);
        key = Arrays.copyOf (key, 32);
        
        final var sessionKey = makeKey (key);
        return blockEncrypt (message, 0, sessionKey);
    }
    
    public static byte [] decrypt (byte [] cipher, byte [] key) {
        key = Arrays.copyOf (key, 32);
        
        final var sessionKey = makeKey (key);
        return blockDecrypt (cipher, 0, sessionKey);
    }
    
    private static synchronized Object makeKey (byte [] k) {
        int length = k.length;
        int k64Cnt = length / 8;
        int subkeyCnt = ROUND_SUBKEYS + 2 * ROUNDS;
        int [] k32e = new int [4];
        int [] k32o = new int [4];
        int [] sBoxKey = new int [4];
        
        int i, j, offset = 0;
        for (i = 0, j = k64Cnt - 1; i < 4 && offset < length; i++, j--) {
            k32e [i] = (k [offset++] & 0xFF) 
                     | (k [offset++] & 0xFF) << 8 
                     | (k [offset++] & 0xFF) << 16
                     | (k [offset++] & 0xFF) << 24;
            k32o [i] = (k [offset++] & 0xFF) 
                     | (k [offset++] & 0xFF) << 8 
                     | (k [offset++] & 0xFF) << 16
                     | (k [offset++] & 0xFF) << 24;
            sBoxKey [j] = RS_MDS_Encode (k32e [i], k32o [i]);
        }
        
        int q, A, B;
        int [] subKeys = new int [subkeyCnt];
        for (i = q = 0; i < subkeyCnt / 2; i++, q += SK_STEP) {
            A = F32 (k64Cnt, q, k32e);
            B = F32 (k64Cnt, q + SK_BUMP, k32o);
            B = B << 8 | B >>> 24;
            A += B;
            subKeys [2 * i] = A;
            A += B;
            subKeys [2 * i + 1] = A << SK_ROTL | A >>> (32 - SK_ROTL);
        }
        
        int k0 = sBoxKey [0];
        int k1 = sBoxKey [1];
        int k2 = sBoxKey [2];
        int k3 = sBoxKey [3];
        int b0, b1, b2, b3;
        int [] sBox = new int [4 * 256];
        for (i = 0; i < 256; i++) {
            b0 = b1 = b2 = b3 = i;
            switch (k64Cnt & 3) {
                case 1:
                    sBox [2 * i]             = MDS [0] [ (Rs [R [0][1]][b0] & 0xFF) ^ b0 (k0)];
                    sBox [2 * i + 1]         = MDS [1] [ (Rs [R [1][1]][b1] & 0xFF) ^ b1 (k0)];
                    sBox [0x200 + 2 * i]     = MDS [2] [ (Rs [R [2][1]][b2] & 0xFF) ^ b2 (k0)];
                    sBox [0x200 + 2 * i + 1] = MDS [3] [ (Rs [R [3][1]][b3] & 0xFF) ^ b3 (k0)];
                    break;
                case 0:
                    b0 = (Rs [R [0][4]][b0] & 0xFF) ^ b0 (k3);
                    b1 = (Rs [R [1][4]][b1] & 0xFF) ^ b1 (k3);
                    b2 = (Rs [R [2][4]][b2] & 0xFF) ^ b2 (k3);
                    b3 = (Rs [R [3][4]][b3] & 0xFF) ^ b3 (k3);
                case 3:
                    b0 = (Rs [R [0][3]][b0] & 0xFF) ^ b0 (k2);
                    b1 = (Rs [R [1][3]][b1] & 0xFF) ^ b1 (k2);
                    b2 = (Rs [R [2][3]][b2] & 0xFF) ^ b2 (k2);
                    b3 = (Rs [R [3][3]][b3] & 0xFF) ^ b3 (k2);
                case 2:
                    sBox [2 * i]             = MDS [0][(Rs [R [0][1]][(Rs [R [0][2]][b0] & 0xFF) ^ b0 (k1)] & 0xFF) ^ b0 (k0)];
                    sBox [2 * i + 1]         = MDS [1][(Rs [R [1][1]][(Rs [R [1][2]][b1] & 0xFF) ^ b1 (k1)] & 0xFF) ^ b1 (k0)];
                    sBox [0x200 + 2 * i]     = MDS [2][(Rs [R [2][1]][(Rs [R [2][2]][b2] & 0xFF) ^ b2 (k1)] & 0xFF) ^ b2 (k0)];
                    sBox [0x200 + 2 * i + 1] = MDS [3][(Rs [R [3][1]][(Rs [R [3][2]][b3] & 0xFF) ^ b3 (k1)] & 0xFF) ^ b3 (k0)];
            }
        }
        
        return new Object [] { sBox, subKeys };
    }
    
    private static byte [] blockEncrypt (byte [] in, int inOffset, Object sessionKey) {
        Object [] sk = (Object []) sessionKey;
        int [] sBox = (int []) sk [0];
        int [] sKey = (int []) sk [1];
        
        int x0 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        int x1 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        int x2 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        int x3 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        
        x0 ^= sKey [INPUT_WHITEN];
        x1 ^= sKey [INPUT_WHITEN + 1];
        x2 ^= sKey [INPUT_WHITEN + 2];
        x3 ^= sKey [INPUT_WHITEN + 3];
        
        int t0, t1;
        int k = ROUND_SUBKEYS;
        for (int R = 0; R < ROUNDS; R += 2) {
            t0 = Fe32 (sBox, x0, 0);
            t1 = Fe32 (sBox, x1, 3);
            x2 ^= t0 + t1 + sKey [k++];
            x2 = x2 >>> 1 | x2 << 31;
            x3 = x3 << 1 | x3 >>> 31;
            x3 ^= t0 + 2 * t1 + sKey [k++];
            
            t0 = Fe32 (sBox, x2, 0);
            t1 = Fe32 (sBox, x3, 3);
            x0 ^= t0 + t1 + sKey [k++];
            x0 = x0 >>> 1 | x0 << 31;
            x1 = x1 << 1 | x1 >>> 31;
            x1 ^= t0 + 2 * t1 + sKey [k++];
        }
        x2 ^= sKey [OUTPUT_WHITEN];
        x3 ^= sKey [OUTPUT_WHITEN + 1];
        x0 ^= sKey [OUTPUT_WHITEN + 2];
        x1 ^= sKey [OUTPUT_WHITEN + 3];
        
        byte [] result = new byte [] { (byte) x2, (byte) (x2 >>> 8), (byte) (x2 >>> 16), (byte) (x2 >>> 24), (byte) x3,
                (byte) (x3 >>> 8), (byte) (x3 >>> 16), (byte) (x3 >>> 24), (byte) x0, (byte) (x0 >>> 8),
                (byte) (x0 >>> 16), (byte) (x0 >>> 24), (byte) x1, (byte) (x1 >>> 8), (byte) (x1 >>> 16),
                (byte) (x1 >>> 24), };
        
        return result;
    }
    
    private static byte [] blockDecrypt (byte [] in, int inOffset, Object sessionKey) {
        Object [] sk = (Object []) sessionKey;
        int [] sBox = (int []) sk [0];
        int [] sKey = (int []) sk [1];
        
        int x2 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        int x3 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        int x0 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        int x1 = (in [inOffset++] & 0xFF) 
               | (in [inOffset++] & 0xFF) << 8 
               | (in [inOffset++] & 0xFF) << 16
               | (in [inOffset++] & 0xFF) << 24;
        
        x2 ^= sKey [OUTPUT_WHITEN];
        x3 ^= sKey [OUTPUT_WHITEN + 1];
        x0 ^= sKey [OUTPUT_WHITEN + 2];
        x1 ^= sKey [OUTPUT_WHITEN + 3];
        
        int k = ROUND_SUBKEYS + 2 * ROUNDS - 1;
        int t0, t1;
        for (int R = 0; R < ROUNDS; R += 2) {
            t0 = Fe32 (sBox, x2, 0);
            t1 = Fe32 (sBox, x3, 3);
            x1 ^= t0 + 2 * t1 + sKey [k--];
            x1 = x1 >>> 1 | x1 << 31;
            x0 = x0 << 1 | x0 >>> 31;
            x0 ^= t0 + t1 + sKey [k--];
            
            t0 = Fe32 (sBox, x0, 0);
            t1 = Fe32 (sBox, x1, 3);
            x3 ^= t0 + 2 * t1 + sKey [k--];
            x3 = x3 >>> 1 | x3 << 31;
            x2 = x2 << 1 | x2 >>> 31;
            x2 ^= t0 + t1 + sKey [k--];
        }
        x0 ^= sKey [INPUT_WHITEN];
        x1 ^= sKey [INPUT_WHITEN + 1];
        x2 ^= sKey [INPUT_WHITEN + 2];
        x3 ^= sKey [INPUT_WHITEN + 3];
        
        byte [] result = new byte [] { (byte) x0, (byte) (x0 >>> 8), (byte) (x0 >>> 16), (byte) (x0 >>> 24), (byte) x1,
                (byte) (x1 >>> 8), (byte) (x1 >>> 16), (byte) (x1 >>> 24), (byte) x2, (byte) (x2 >>> 8),
                (byte) (x2 >>> 16), (byte) (x2 >>> 24), (byte) x3, (byte) (x3 >>> 8), (byte) (x3 >>> 16),
                (byte) (x3 >>> 24), };
        
        return result;
    }
    
}
