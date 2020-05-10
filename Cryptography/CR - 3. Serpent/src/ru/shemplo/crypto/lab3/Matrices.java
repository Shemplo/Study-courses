package ru.shemplo.crypto.lab3;

import static ru.shemplo.crypto.lab3.Utils.*;

public class Matrices {
    
    private static final int [] intR0 = {
        0xA9, 0x67, 0xB3, 0xE8, 0x04, 0xFD, 0xA3, 0x76, 
        0x9A, 0x92, 0x80, 0x78, 0xE4, 0xDD, 0xD1, 0x38, 
        0x0D, 0xC6, 0x35, 0x98, 0x18, 0xF7, 0xEC, 0x6C, 
        0x43, 0x75, 0x37, 0x26, 0xFA, 0x13, 0x94, 0x48, 
        0xF2, 0xD0, 0x8B, 0x30, 0x84, 0x54, 0xDF, 0x23, 
        0x19, 0x5B, 0x3D, 0x59, 0xF3, 0xAE, 0xA2, 0x82, 
        0x63, 0x01, 0x83, 0x2E, 0xD9, 0x51, 0x9B, 0x7C, 
        0xA6, 0xEB, 0xA5, 0xBE, 0x16, 0x0C, 0xE3, 0x61, 
        0xC0, 0x8C, 0x3A, 0xF5, 0x73, 0x2C, 0x25, 0x0B, 
        0xBB, 0x4E, 0x89, 0x6B, 0x53, 0x6A, 0xB4, 0xF1, 
        0xE1, 0xE6, 0xBD, 0x45, 0xE2, 0xF4, 0xB6, 0x66, 
        0xCC, 0x95, 0x03, 0x56, 0xD4, 0x1C, 0x1E, 0xD7, 
        0xFB, 0xC3, 0x8E, 0xB5, 0xE9, 0xCF, 0xBF, 0xBA, 
        0xEA, 0x77, 0x39, 0xAF, 0x33, 0xC9, 0x62, 0x71, 
        0x81, 0x79, 0x09, 0xAD, 0x24, 0xCD, 0xF9, 0xD8, 
        0xE5, 0xC5, 0xB9, 0x4D, 0x44, 0x08, 0x86, 0xE7, 
        0xA1, 0x1D, 0xAA, 0xED, 0x06, 0x70, 0xB2, 0xD2, 
        0x41, 0x7B, 0xA0, 0x11, 0x31, 0xC2, 0x27, 0x90, 
        0x20, 0xF6, 0x60, 0xFF, 0x96, 0x5C, 0xB1, 0xAB, 
        0x9E, 0x9C, 0x52, 0x1B, 0x5F, 0x93, 0x0A, 0xEF, 
        0x91, 0x85, 0x49, 0xEE, 0x2D, 0x4F, 0x8F, 0x3B, 
        0x47, 0x87, 0x6D, 0x46, 0xD6, 0x3E, 0x69, 0x64, 
        0x2A, 0xCE, 0xCB, 0x2F, 0xFC, 0x97, 0x05, 0x7A, 
        0xAC, 0x7F, 0xD5, 0x1A, 0x4B, 0x0E, 0xA7, 0x5A, 
        0x28, 0x14, 0x3F, 0x29, 0x88, 0x3C, 0x4C, 0x02, 
        0xB8, 0xDA, 0xB0, 0x17, 0x55, 0x1F, 0x8A, 0x7D, 
        0x57, 0xC7, 0x8D, 0x74, 0xB7, 0xC4, 0x9F, 0x72, 
        0x7E, 0x15, 0x22, 0x12, 0x58, 0x07, 0x99, 0x34, 
        0x6E, 0x50, 0xDE, 0x68, 0x65, 0xBC, 0xDB, 0xF8, 
        0xC8, 0xA8, 0x2B, 0x40, 0xDC, 0xFE, 0x32, 0xA4, 
        0xCA, 0x10, 0x21, 0xF0, 0xD3, 0x5D, 0x0F, 0x00, 
        0x6F, 0x9D, 0x36, 0x42, 0x4A, 0x5E, 0xC1, 0xE0
    };
    
    public static final byte [] R0 = new byte [intR0.length];
    
    static {
        for (int i = 0; i < intR0.length; i++) {
            R0 [i] = (byte) intR0 [i];
        }
    }
    
    private static final int [] intR1 = {
        0x75, 0xF3, 0xC6, 0xF4, 0xDB, 0x7B, 0xFB, 0xC8, 
        0x4A, 0xD3, 0xE6, 0x6B, 0x45, 0x7D, 0xE8, 0x4B, 
        0xD6, 0x32, 0xD8, 0xFD, 0x37, 0x71, 0xF1, 0xE1, 
        0x30, 0x0F, 0xF8, 0x1B, 0x87, 0xFA, 0x06, 0x3F, 
        0x5E, 0xBA, 0xAE, 0x5B, 0x8A, 0x00, 0xBC, 0x9D, 
        0x6D, 0xC1, 0xB1, 0x0E, 0x80, 0x5D, 0xD2, 0xD5, 
        0xA0, 0x84, 0x07, 0x14, 0xB5, 0x90, 0x2C, 0xA3, 
        0xB2, 0x73, 0x4C, 0x54, 0x92, 0x74, 0x36, 0x51, 
        0x38, 0xB0, 0xBD, 0x5A, 0xFC, 0x60, 0x62, 0x96, 
        0x6C, 0x42, 0xF7, 0x10, 0x7C, 0x28, 0x27, 0x8C, 
        0x13, 0x95, 0x9C, 0xC7, 0x24, 0x46, 0x3B, 0x70, 
        0xCA, 0xE3, 0x85, 0xCB, 0x11, 0xD0, 0x93, 0xB8, 
        0xA6, 0x83, 0x20, 0xFF, 0x9F, 0x77, 0xC3, 0xCC, 
        0x03, 0x6F, 0x08, 0xBF, 0x40, 0xE7, 0x2B, 0xE2, 
        0x79, 0x0C, 0xAA, 0x82, 0x41, 0x3A, 0xEA, 0xB9, 
        0xE4, 0x9A, 0xA4, 0x97, 0x7E, 0xDA, 0x7A, 0x17, 
        0x66, 0x94, 0xA1, 0x1D, 0x3D, 0xF0, 0xDE, 0xB3, 
        0x0B, 0x72, 0xA7, 0x1C, 0xEF, 0xD1, 0x53, 0x3E, 
        0x8F, 0x33, 0x26, 0x5F, 0xEC, 0x76, 0x2A, 0x49, 
        0x81, 0x88, 0xEE, 0x21, 0xC4, 0x1A, 0xEB, 0xD9, 
        0xC5, 0x39, 0x99, 0xCD, 0xAD, 0x31, 0x8B, 0x01, 
        0x18, 0x23, 0xDD, 0x1F, 0x4E, 0x2D, 0xF9, 0x48, 
        0x4F, 0xF2, 0x65, 0x8E, 0x78, 0x5C, 0x58, 0x19, 
        0x8D, 0xE5, 0x98, 0x57, 0x67, 0x7F, 0x05, 0x64, 
        0xAF, 0x63, 0xB6, 0xFE, 0xF5, 0xB7, 0x3C, 0xA5, 
        0xCE, 0xE9, 0x68, 0x44, 0xE0, 0x4D, 0x43, 0x69, 
        0x29, 0x2E, 0xAC, 0x15, 0x59, 0xA8, 0x0A, 0x9E, 
        0x6E, 0x47, 0xDF, 0x34, 0x35, 0x6A, 0xCF, 0xDC, 
        0x22, 0xC9, 0xC0, 0x9B, 0x89, 0xD4, 0xED, 0xAB, 
        0x12, 0xA2, 0x0D, 0x52, 0xBB, 0x02, 0x2F, 0xA9, 
        0xD7, 0x61, 0x1E, 0xB4, 0x50, 0x04, 0xF6, 0xC2, 
        0x16, 0x25, 0x86, 0x56, 0x55, 0x09, 0xBE, 0x91
    };
    
    
    public static final byte [] R1 = new byte [intR1.length];
    
    static {
        for (int i = 0; i < intR1.length; i++) {
            R1 [i] = (byte) intR1 [i];
        }
    }
    
    public static final byte [][] Rs = { R0, R1 };
    
    public static final int [][] R = {
        {1, 0, 0, 1, 1},
        {0, 0, 1, 1, 0},
        {1, 1, 0, 0, 0},
        {0, 1, 1, 0, 1}
    };
    
    public static final int [][] MDS = new int [4][256];
    
    static {
        int [] m1 = new int [2], mX = new int [2], mY = new int [2];
        
        for (int i = 0; i < MDS [0].length; i++) {
            int j = R0 [i] & 0xFF;
            
            m1 [0] = j;
            mX [0] = Mx_X (j) & 0xFF;
            mY [0] = Mx_Y (j) & 0xFF;
            
            j = R1 [i] & 0xFF;
            
            m1 [1] = j;
            mX [1] = Mx_X (j) & 0xFF;
            mY [1] = Mx_Y (j) & 0xFF;
            
            for (int k = 0; k < MDS.length; k++) {
                MDS [k][i] = m1 [R [k][0]] | mX [R [k][0]] << 8 | mY [R [k][0]] << 16 | mY [R [k][0]] << 24;                
            }
        }
      }
    
}