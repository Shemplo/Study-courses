package ru.shemplo.crypto.lab3;

import static ru.shemplo.crypto.lab3.Matrices.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Utils {
    
    private static final int GF256_FDBK_2 = 0x169 / 2;
    private static final int GF256_FDBK_4 = 0x169 / 4;
    private static final int RS_GF_FDBK = 0x14D;
    
    public static String readInput () throws IOException {
        var r = new InputStreamReader (System.in);
        var br = new BufferedReader (r);
        
        return br.readLine ();
    }
    
    public static long getLongFromBytes (byte [] array, int offset) {
        long l = 0;
        for (int i = 0; i < 8; i++) {
            byte value;
            if ( (offset + i) < array.length) {
                value = array [offset + i];
            } else {
                value = 0;
            }
            l = l << 8 | (value & 0xFFL);
        }
        return l;
    }
    
    public static void getBytesFromLong (byte [] array, int offset, long l) {
        for (int i = 7; i > -1; i--) {
            if ( (offset + i) < array.length) {
                array [offset + i] = (byte) (l & 0xFF);
                l = l >> 8;
            } else {
                break;
            }
        }
    }
    
    public static int LFSR1 (int x) {
        return (x >> 1) ^ ( (x & 0x01) != 0 ? GF256_FDBK_2 : 0);
    }
    
    public static int LFSR2 (int x) {
        return (x >> 2) ^ ( (x & 0x02) != 0 ? GF256_FDBK_2 : 0) ^ ( (x & 0x01) != 0 ? GF256_FDBK_4 : 0);
    }
    
    public static int Mx_X (int x) {
        return x ^ LFSR2 (x);
    }
    
    public static int Mx_Y (int x) {
        return x ^ LFSR1 (x) ^ LFSR2 (x);
    }
    
    public static int b0 (int x) {
        return x & 0xFF;
    }
    
    public static int b1 (int x) {
        return (x >>> 8) & 0xFF;
    }
    
    public static int b2 (int x) {
        return (x >>> 16) & 0xFF;
    }
    
    public static int b3 (int x) {
        return (x >>> 24) & 0xFF;
    }
    
    public static int RS_MDS_Encode (int k0, int k1) {
        int r = k1;
        for (int i = 0; i < 4; i++) {
            r = RS_rem (r);
        }
        r ^= k0;
        for (int i = 0; i < 4; i++) {
            r = RS_rem (r);
        }
        return r;
    }
    
    public static int RS_rem (int x) {
        int b = (x >>> 24) & 0xFF;
        int g2 = ((b << 1) ^ ((b & 0x80) != 0 ? RS_GF_FDBK : 0)) & 0xFF;
        int g3 = (b >>> 1) ^ ((b & 0x01) != 0 ? (RS_GF_FDBK >>> 1) : 0) ^ g2;
        return (x << 8) ^ (g3 << 24) ^ (g2 << 16) ^ (g3 << 8) ^ b;
    }
    
    public static int F32 (int k64Cnt, int x, int [] k32) {
        int b0 = b0 (x);
        int b1 = b1 (x);
        int b2 = b2 (x);
        int b3 = b3 (x);
        int k0 = k32 [0];
        int k1 = k32 [1];
        int k2 = k32 [2];
        int k3 = k32 [3];
        
        int result = 0;
        switch (k64Cnt & 3) {
            case 1:
                result = MDS [0][(Rs [R [0][1]] [b0] & 0xFF) ^ b0 (k0)] 
                       ^ MDS [1][(Rs [R [1][1]] [b1] & 0xFF) ^ b1 (k0)] 
                       ^ MDS [2][(Rs [R [2][1]] [b2] & 0xFF) ^ b2 (k0)] 
                       ^ MDS [3][(Rs [R [3][1]] [b3] & 0xFF) ^ b3 (k0)];
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
                result = MDS [0][(Rs [R [0][1]][(Rs [R [0][2]] [b0] & 0xFF) ^ b0 (k1)] & 0xFF) ^ b0 (k0)]
                       ^ MDS [1][(Rs [R [1][1]][(Rs [R [1][2]] [b1] & 0xFF) ^ b1 (k1)] & 0xFF) ^ b1 (k0)]
                       ^ MDS [2][(Rs [R [2][1]][(Rs [R [2][2]] [b2] & 0xFF) ^ b2 (k1)] & 0xFF) ^ b2 (k0)]
                       ^ MDS [3][(Rs [R [3][1]][(Rs [R [3][2]] [b3] & 0xFF) ^ b3 (k1)] & 0xFF) ^ b3 (k0)];
                break;
        }
        return result;
    }
    
    public static int Fe32 (int [] sBox, int x, int R) {
        return sBox [2 * _b (x, R)] 
             ^ sBox [2 * _b (x, R + 1) + 1] 
             ^ sBox [0x200 + 2 * _b (x, R + 2)]
             ^ sBox [0x200 + 2 * _b (x, R + 3) + 1];
    }
    
    private static int _b (int x, int N) {
        int result = 0;
        switch (N % 4) {
            case 0:
                result = b0 (x);
                break;
            case 1:
                result = b1 (x);
                break;
            case 2:
                result = b2 (x);
                break;
            case 3:
                result = b3 (x);
                break;
        }
        return result;
    }
    
}
