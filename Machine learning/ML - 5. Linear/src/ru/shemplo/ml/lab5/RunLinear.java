package ru.shemplo.ml.lab5;

import static java.lang.Double.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.Arrays;
import java.util.Locale;
import java.util.StringJoiner;
import java.util.StringTokenizer;

public class RunLinear {
    
    private static final double TAU = 0.0095,
                                EPS = 0.000000001;
    
    private static double [][] matrix;
    private static double []   ys;
    
    private static int m = 0, n = 0;
    
    public static void main (String ... args) throws IOException {
        Locale.setDefault (Locale.ENGLISH);
        
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
        ) {
            m = Integer.parseInt (br.readLine ());
            n = Integer.parseInt (br.readLine ());
            
            matrix = new double [n][m + 1];
            ys     = new double [n];
            
            for (int i = 0; i < n; i++) {
                StringTokenizer st = new StringTokenizer (br.readLine ());
                for (int j = 0; j < m; j++) {
                    matrix [i][j] = parseDouble (st.nextToken ());
                }
                
                ys [i] = parseDouble (st.nextToken ());
                matrix [i][m] = 1.0d;
            }
            
            //printMatrix ("Original", matrix);
            double [][] transposed = transposeMatrix (matrix);
            //printMatrix ("Transposed", transposed);
            double [][] multiplied = multiplyMatrices (transposed, matrix);
            //printMatrix ("Multiplied", multiplied);
            
            for (int i = 0; i < multiplied.length; i++) {
                multiplied [i][i] += TAU; // make diagonal priority
            }
            
            double [][] inversed = inverseMatrix (multiplied);
            //printMatrix ("Inversed", inversed);
            multiplied = multiplyMatrices (inversed, transposed);
            //printMatrix ("Multiplied 2", multiplied);
            
            double [] result = multiplyVectorRight (multiplied, ys);
            for (int i = 0; i < result.length; i++) {
                if (Double.isFinite (result [i])) {
                    System.out.println (String.format ("%.10f", result [i]));
                } else {
                    System.out.println (0d);
                }
            }
        }
    }
    
    public static void printMatrix (String name, double [][] matrix) {
        if (name != null && name.length () > 0) {
            int h = matrix.length, w = matrix [0].length;
            System.out.println (String.format ("%s [%dx%d]:", name, h, w));
        }
        
        Arrays.asList (matrix).forEach (r -> {
            StringJoiner sj = new StringJoiner (" ");
            
            Double [] row = new Double [r.length];
            for (int i = 0; i < r.length; i++) {
                row [i] = r [i];
            }
            
            Arrays.asList (row).stream ().map (v -> String.format ("%3.3f", v)).forEach (sj::add);
            System.out.println (String.format ("║%s║", sj.toString ()));
        });
    }
    
    private static double [][] transposeMatrix (double [][] matrix) {
        double [][] transposed = new double [matrix [0].length][matrix.length];
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix [i].length; j++) {
                transposed [j][i] = matrix [i][j];
            }
        }
        
        return transposed;
    }
    
    private static double [][] multiplyMatrices (double [][] left, double [][] right) {
        double [][] multiplied = new double [left.length][right [0].length];
        for (int i = 0; i < left.length; i++) {
            for (int j = 0; j < right [0].length; j++) {
                for (int k = 0; k < left [i].length; k++) {
                    multiplied [i][j] += left  [i][k] 
                                       * right [k][j];
                }
            }
        }
        
        return multiplied;
    }
    
    private static double [][] inverseMatrix (double [][] matrix) {
        final int height = matrix.length, width = matrix [0].length 
                                                + height;
        
        double [][] mated = new double [height][width];
        for (int i = 0; i < matrix.length; i++) { // making (P | I) matrix
            System.arraycopy (matrix [i], 0, mated [i], 0, matrix [0].length);
            mated [i][i + matrix [0].length] = 1.0d; 
        }
        
        // forward
        
        for (int i = 0; i < matrix.length; i++) {
            int fromRow = i, fromColumn = fromRow;
            
            double tmp = mated [fromRow][fromColumn];
            mated [fromRow][fromColumn] = 1.0d;
            
            for (int j = fromColumn + 1; j < width; j++) {
                mated [fromRow][j] /= tmp;
            }
            
            for (int j = fromRow + 1; j < height; j++) {
                if (Math.abs (mated [j][fromColumn]) < EPS) {
                    continue; // value is too small
                }
                
                tmp = mated [j][fromColumn];
                mated [j][fromColumn] = 0d;
                
                for (int k = fromColumn + 1; k < width; k++) {
                    mated [j][k] -= mated [fromRow][k] * tmp;
                }
            }
        }
        
        // back
        
        for (int i = height - 1; i >= 0; i--) {
            int fromRow = i, fromColumn = fromRow;
            for (int j = fromRow - 1; j >= 0; j--) {
                if (Math.abs (mated [j][fromColumn]) < EPS) {
                    continue; // value is too small
                }
                
                double tmp = mated [j][fromColumn];
                for (int k = width - 1; k > j; k--) {
                    mated [j][k] -= mated [fromRow][k] * tmp;
                }
            }
        }
        
        // inverse
        
        double [][] inversed = new double [matrix.length][matrix [0].length];
        for (int i = 0; i < matrix.length; i++) {
            System.arraycopy (mated [i], matrix [0].length, 
                           inversed [i], 0, matrix.length);
        }
        
        return inversed;
    }
    
    private static double [] multiplyVectorRight (double [][] matrix, double [] vector) {
        double [] multiplied = new double [matrix.length];
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix [i].length; j++) {
                multiplied [i] += matrix [i][j] * vector [j];
            }
        }
        
        return multiplied;
    }
    
}
