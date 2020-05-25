package ru.shemplo.crypto.lab8;

import static java.lang.Math.*;
import static java.util.Arrays.*;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;

public class Keccak {
    
    private static BigInteger MAX_MOD_M_1 = Utils.MAX_MOD.subtract (BigInteger.ONE);

    private final int r, d, out;
    
    public Keccak (int r, int d, int out) {
        this.r = r; this.d = d; this.out = out;
    }
    
    public byte [] digest (final byte [] message) {
        final int [] p = new int [message.length];
        final int [] s = new int [1600 >>> 3];
        
        for (int i = 0; i < message.length; i++) {
            p [i] = Byte.toUnsignedInt (message [i]);
        }

        final int rBytes = r >>> 3;
        int blockSize = 0, inputOffset = 0;

        // Absorbing phase
        while (inputOffset < p.length) {
            blockSize = min (p.length - inputOffset, rBytes);
            for (int i = 0; i < blockSize; i++) {
                s[i] ^= p [i + inputOffset];
            }

            inputOffset += blockSize;
            if (blockSize == rBytes) {
                doKeccakF(s);
                blockSize = 0;
            }
        }

        // Padding phase
        s [blockSize] = s [blockSize] ^ d;
        if ((d & 0x80) != 0 && blockSize == (rBytes - 1)) {
            doKeccakF (s);
        }

        s [rBytes - 1] ^= 0x80;
        doKeccakF (s);
        
        // Squeezing phase
        final var byteResults = new ByteArrayOutputStream ();
        int tOutputLen = out >>> 3;
        
        while (tOutputLen > 0) {
            blockSize = min (tOutputLen, rBytes);
            for (int i = 0; i < blockSize; i++) {
                byteResults.write ((byte) s [i]);
            }
            
            tOutputLen -= blockSize;
            if (tOutputLen > 0) {
                doKeccakF (s);
            }
        }
        
        return byteResults.toByteArray ();
    }

    private void doKeccakF (final int[] s) {
        BigInteger [][] a = new BigInteger [5][5];

        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                a[i][j] = Utils.concatBytes (s, 8 * (i + 5 * j), 8);
            }
        }
        roundB (a);

        fill (s, 0);
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                Utils.splitBytes (a [i][j], s, 8 * (i + 5 * j), 8);
            }
        }
    }
    
    private void roundB (final BigInteger [][] state) {
        for (int round = 0, LFSRstate = 1; round < 24; round++) {
            BigInteger [] C = new BigInteger [5];
            BigInteger [] D = new BigInteger [5];
            
            // θ step (XOR in pair columns)
            for (int i = 0; i < 5; i++) {
                C [i] = state [i][0].xor (state [i][1]).xor (state [i][2]).xor (state [i][3]).xor (state [i][4]);
            }
            
            for (int i = 0; i < 5; i++) {
                D [i] = C [(i + 4) % 5].xor (Utils.leftRotate (C [(i + 1) % 5], 1, 64));
            }
            
            for (int i = 0; i < 5; i++) {
                for (int j = 0; j < 5; j++) {
                    state [i][j] = state [i][j].xor (D [i]);
                }
            }
            
            // ρ and π steps (shifts in the row of 5x5)
            int x = 1, y = 0;
            BigInteger current = state [x][y];
            for (int i = 0; i < 24; i++) {
                int tX = x; x = y;
                y = (2 * tX + 3 * y) % 5;
                
                BigInteger shiftValue = current;
                current = state [x][y];
                
                state [x][y] = Utils.leftRotate (shiftValue, (i + 1) * (i + 2) / 2, 64);
            }
            
            // χ step (permutations in 5x5)
            for (int j = 0; j < 5; j++) {
                BigInteger [] t = new BigInteger [5];
                for (int i = 0; i < 5; i++) {
                    t [i] = state [i][j];
                }
                
                for (int i = 0; i < 5; i++) {
                    BigInteger invertVal = t [(i + 1) % 5].xor (MAX_MOD_M_1);
                    state [i][j] = t [i].xor (invertVal.and (t [(i + 2) % 5]));
                }
            }
            
            // ι step (round-dependent transformation)
            for (int i = 0; i < 7; i++) {
                LFSRstate = ((LFSRstate << 1) ^ ((LFSRstate >> 7) * 0x71)) % 256;
                int bitPosition = (1 << i) - 1;
                if ((LFSRstate & 2) != 0) {
                    state [0][0] = state [0][0].xor (BigInteger.ONE.shiftLeft (bitPosition));
                }
            }
        }
    }
    
}
