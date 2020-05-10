package ru.shemplo.crypto.lab3;

import static ru.shemplo.crypto.lab3.Matrices.*;

// A3,A8
public class COMP128Cipher {
    
    public int [] encrypt (int [] rand, int [] key) {
        int [] x = new int [32], bits = new int [128];
        int [] out = new int [12];
        
        System.arraycopy (rand, 0, x, 16, 16);
        
        for (int i = 0; i < 8; i++) {
            System.arraycopy (key, 0, x, 0, 16);
            
            for (int j = 0; j < 5; j++) {
                for (int k = 0; k < (1 << j); k++) {
                    for (int a = 0; a < (1 << (4 - j)); a++) {
                        int m = a + k * (1 << (5 - j));
                        int n = m + (1 << (4 - j));
                        int y = (x [m] + 2 * x [n]) % TABLE [j].length;
                        int z = (2 * x [m] + x [n]) % TABLE [j].length;
                        x [m] = TABLE [j][y];
                        x [n] = TABLE [j][z];
                    }
                }
            }
            
            for (int j = 0; j < 32; j++) {
                for (int k = 0; k < 4; k++) {
                    bits [4 * j + k] = (x [j] >> (3 - k)) & 1;
                }
            }
            
            if (i < 7) {
                for (int j = 0; j < 16; j++) {
                    x [j + 16] = 0;
                    for (int k = 0; k < 8; k++) {
                        int bit = ((8 * j + k) * 17) % 128;
                        x [j + 16] |= ((bits [bit] << (7 - k)) & 0xFF);
                    }
                }
            }
        }
        
        for (int i = 0; i < 4; i++) {
            out [i] = ((x [2 * i] << 4) | x [2 * i + 1]) & 0xFF;
        }
        for (int i = 0; i < 6; i++) {
            out [4 + i] = ((x [2 * i + 18] << 6) | (x [2 * i + 18 + 1] << 2) | (x [2 * i + 18 + 2] >> 2)) & 0xFF;
        }
        
        out [4 + 6] = ((x [2 * 6 + 18] << 6) | (x [2 * 6 + 18 + 1] << 2)) & 0xFF;
        out [4 + 7] = 0;
        
        return out;
    }
    
}
