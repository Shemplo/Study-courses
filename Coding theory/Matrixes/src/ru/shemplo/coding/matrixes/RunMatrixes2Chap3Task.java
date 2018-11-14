package ru.shemplo.coding.matrixes;

import java.util.Arrays;
import java.util.Locale;
import java.util.Map;
import java.util.StringJoiner;

public class RunMatrixes2Chap3Task /*7 Task too*/ extends RunMatrixes {
    
    private static final Integer [][][] Gs = {
        {
            {1, 0, 1, 1, 0, 0},
            {0, 1, 0, 1, 1, 0},
            {0, 0, 1, 0, 1, 1}
        }, {
            {0, 0, 1, 0, 1, 1},
            {0, 1, 0, 1, 1, 0},
            {1, 0, 1, 1, 0, 0}
        }, {
            {0, 1, 0, 1, 1, 0},
            {1, 0, 1, 1, 0, 1}
        }, {
            {0, 1, 0, 1, 1, 0},
            {1, 0, 1, 1, 0, 1},
            {1, 1, 0, 1, 0, 0}
        }, {
            {1, 1, 1, 1, 1, 1},
            {0, 1, 0, 1, 1, 0},
            {0, 0, 1, 0, 1, 1}
        }, {
            {0, 0, 1, 0, 1, 1},
            {1, 1, 1, 1, 1, 1},
            {1, 0, 1, 1, 0, 0}
        }, {
            {0, 1, 0, 1, 1, 0},
            {1, 1, 1, 1, 1, 1}
        }
    };
    
    private static final Integer [][][] Hs = {
        {
            {0, 1, 0, 1, 1, 0, 0},
            {1, 0, 1, 1, 0, 1, 0},
            {1, 1, 1, 1, 1, 1, 1}
        }, {
            {1, 1, 1, 0, 1, 0, 0},
            {0, 1, 1, 1, 0, 1, 0},
            {0, 0, 1, 1, 1, 0, 1}
        }, {
            {1, 1, 1, 0, 1, 0, 0},
            {0, 1, 1, 1, 0, 1, 0},
            {1, 1, 1, 1, 1, 1, 1}
        }
    };
    
    public static void main (String ... args) {
        Locale.setDefault (Locale.ENGLISH);
        Arrays.stream (Gs).forEach (RunMatrixes2Chap3Task::processGMatrix);
        Arrays.stream (Hs).forEach (RunMatrixes2Chap3Task::processHMatrix);
    }
    
    private static final void processGMatrix (Integer [][] matrix) {
        int k = matrix.length, n = matrix [0].length;
        System.out.println (String.format ("k = %d, n = %d, R = %.4f", 
                                                  k, n, 1.0 * k / n));
        RunMatrixes.printMatrix ("G", matrix);
        
        for (int i = 0; i < k; i++) {
            if (matrix [i][i] == 0) {
                for (int j = 0; j < k; j++) {
                    if (i == j) { continue; }
                    
                    if (matrix [j][i] != 0) {
                        addRow2Row (matrix, j, i);
                        break;
                    }
                }
            }
            
            for (int j = 0; j < k; j++) {
                if (i == j) { continue; }
                
                if (matrix [j][i] != 0) {
                    addRow2Row (matrix, i, j);
                }
            }
        }
        
        RunMatrixes.printMatrix ("G in SCF", matrix);
        
        Integer [][] h = new Integer [n - k][n];
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < n - k; j++) {
                h [j][i] = matrix [i][k + j];
            }
        }
        for (int i = 0; i < n - k; i++) {
            for (int j = 0; j < n - k; j++) {
                h [i][k + j] = i == j ? 1 : 0;
            }
        }
        
        RunMatrixes.printMatrix ("H in SCF", h);
        
        Map <Integer, Integer []> sindroms = generateSyndromTable (h);
        int totalSyndroms = 1 << n - k;
        
        System.out.println ("Syndromes table:");
        for (int i = 0; i < totalSyndroms; i++) {
            String sindrome = Integer.toBinaryString (i);
            Integer [] fix = sindroms.get (i);
            if (fix == null) { continue; }
            
            StringJoiner sj = new StringJoiner (" ");
            Arrays.stream (fix).map (v -> String.format ("%1d", v)).forEach (sj::add);
            System.out.println (String.format ("%" + (n - k) + "s | ║%s║", 
                                           sindrome, sj.toString ()));
        }
        
        System.out.println ();
    }
    
    private static final void processHMatrix (Integer [][] matrix) {
        int nmk = matrix.length, n = matrix [0].length;
        Integer [][] HinSCF = new Integer [nmk][];
        for (int i = 0; i < nmk; i++) {
            HinSCF [i] = Arrays.copyOf (matrix [i], n);
        }
        
        for (int i = 0; i < nmk; i++) {
            if (HinSCF [nmk - i - 1][n - i - 1] == 0) {
                for (int j = 0; j < nmk; j++) {
                    if (i == j) { continue; }
                    
                    if (HinSCF [nmk - j - 1][n - i - 1] != 0) {
                        addRow2Row (HinSCF, nmk - j - 1, nmk - i - 1);
                        break;
                    }
                }
            }
            
            for (int j = 0; j < nmk; j++) {
                if (i == j) { continue; }
                
                if (HinSCF [nmk - j - 1][n - i - 1] != 0) {
                    addRow2Row (HinSCF, nmk - i - 1, nmk - j - 1);
                }
            }
        }
        
        Integer [][] g = new Integer [n - nmk][n];
        int k = n - nmk;
        
        for (int i = 0; i < g.length; i++) {
            for (int j = 0; j < g [i].length; j++) {
                g [i][j] = new Integer (0);
            }
        }
        
        for (int i = 0; i < g.length; i++) {
            g [i][i] = 1;
            for (int j = 0; j < HinSCF.length; j++) {
                g [i][k + j] = HinSCF [j][i];
            }
        }
        
        processGMatrix (g);
    }
    
}
