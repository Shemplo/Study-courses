package ru.shemplo.coding.matrixes;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.StringJoiner;

public class RunMatrixes {
    
    private static final Random RAND = new Random ();
    private static final int MOD = 2;
    
    private static final Integer [][] H = {
        {0, 1, 0, 0, 0, 1, 0, 1, 1, 1},
        {1, 1, 0, 1, 0, 0, 1, 0, 0, 1},
        {1, 0, 0, 1, 1, 1, 0, 0, 1, 1},
        {1, 0, 1, 0, 1, 1, 0, 1, 0, 1}
    };
    private static Integer [][] g, HinSCF, GinMSF;
    
    private static int n, k, nmk;
    private static double R;
    
    public static void main (String ... args) {
        int rowH = H.length, colH = H [0].length;
        System.out.println ("Found checking matrinx " 
                         + rowH + "x" + colH);
        printMatrix ("H", H);
        
        n = colH; k = n - rowH; nmk = n - k;
        System.out.println (String.format ("n = %d, k = %d", n, k));
        
        R = 1.0 * k / n;
        System.out.println (String.format (Locale.ENGLISH, 
                                            "R = %.1f", R));
        
        
        transormH2SCF ();
        printMatrix ("H", H);
        printMatrix ("H in SCF", HinSCF);
        
        g = new Integer [k][n];
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
        printMatrix ("G in SCF", g);
        
        Integer [] message = new Integer [k];
        for (int i = 0; i < message.length; i++) {
            message [i] = new Integer (RAND.nextBoolean () ? 1 : 0);
        }
        System.out.println ("Message:");
        System.out.println (Arrays.toString (message));
        
        Integer [] code = new Integer [n];
        for (int i = 0; i < code.length; i++) {
            code [i] = mulColOnMatrix (message, g, i);
        }
        System.out.println ("Code:");
        System.out.println (Arrays.toString (code));
        
        System.err.println ("For information set you need to select `k` "
                         + "lineary independent columns in H");
        System.err.flush ();
        
        Map <Integer, Integer []> sindroms = generateSyndromTable (H);
        
        int totalSyndroms = 1 << nmk;
        for (int i = 0; i < totalSyndroms; i++) {
            String sindrome = Integer.toBinaryString (i);
            Integer [] fix = sindroms.get (i);
            if (fix == null) { continue; }
            
            StringJoiner sj = new StringJoiner (" ");
            Arrays.stream (fix).map (v -> String.format ("%1d", v)).forEach (sj::add);
            System.out.println (String.format ("%" + nmk + "s | ║%s║", 
                                           sindrome, sj.toString ()));
        }
        System.err.println ("The rest of missed syndroms necessary to do by hands");
        System.err.flush ();
        
        transformG2MSF ();
        printMatrix ("G in MSF", GinMSF);
        
        Integer [] profile = getComplexityProfile ();
        System.out.println ("Complexity profile:");
        System.out.println (Arrays.toString (profile));
    }
    
    public static void printMatrix (String name, Integer [][] matrix) {
        if (name != null && name.length () > 0) {
            int h = matrix.length, w = matrix [0].length;
            System.out.println (String.format ("%s [%dx%d]:", name, h, w));
        }
        
        Arrays.asList (matrix).forEach (r -> {
            StringJoiner sj = new StringJoiner (" ");
            Arrays.asList (r).stream ().map (v -> String.format ("%1d", v)).forEach (sj::add);
            System.out.println (String.format ("║%s║", sj.toString ()));
        });
    }
    
    public static void addRow2Row (Integer [][] matrix, int row, int dest) {
        for (int i = 0; i < matrix [row].length; i++) {
            matrix [dest][i] = (matrix [dest][i] + matrix [row][i]) % MOD;
        }
    }
    
    private static Integer [] addVector2Vector (Integer [] f, Integer [] s) {
        Integer [] result = new Integer [f.length];
        for (int i = 0; i < result.length; i++) {
            result [i] = (f [i] + s [i]) % MOD; 
        }
        
        return result;
    }
    
    private static Integer mulVectorOnVector (Integer [] f, Integer [] s) {
        int result = 0;
        for (int i = 0; i < f.length; i++) {
            result += f [i] * s [i];
        }
        
        return new Integer (result % MOD);
    }
    
    private static Integer mulColOnMatrix (Integer [] vector, Integer [][] matrix, int column) {
        int result = 0;
        for (int i = 0; i < vector.length; i++) {
            result += matrix [i][column] * vector [i];
        }
        
        return new Integer (result % MOD);
    }
    
    private static void transormH2SCF () {
        HinSCF = new Integer [nmk][];
        for (int i = 0; i < nmk; i++) {
            HinSCF [i] = Arrays.copyOf (H [i], n);
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
    }
    
    private static void transformG2MSF () {
        GinMSF = new Integer [k][];
        for (int i = 0; i < k; i++) {
            GinMSF [i] = Arrays.copyOf (g [i], n);
        }
        
        printMatrix ("G tmp", GinMSF);
        
        for (int i = 0; i < n; i++) {
            int row = -1;
            for (int j = i; j < k; j++) {
                if (GinMSF [k - j - 1][n - i - 1] != 0) {
                    row = k - j - 1;
                    break;
                }
            }
            
            for (int j = row - 1; j >= 0; j--) {
                if (GinMSF [j][n - i - 1] != 0) {
                    addRow2Row (GinMSF, row, j);
                }
            }
        }
    }
    
    private static Integer [] getComplexityProfile () {
        Integer [] profile = new Integer [n + 2];
        profile [0] = profile [n + 1] = 1;
        int [] over = new int [k];
        
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < n; j++) {
                if (GinMSF [i][j] != 0) {
                    over [i] = j;
                }
            }
        }
        
        for (int i = 0; i < n; i++) {
            int notActive = 0;
            for (int j = 0; j < k; j++) {
                notActive += i >= over [j] ? 1 : 0;
            }
            
            int pow = Math.max (0, Math.min (i, k) - notActive + 1);
            profile [i + 1] = 1 << pow;
        }
        
        return profile;
    }
    
    public static Map <Integer, Integer []> generateSyndromTable (final Integer [][] H) {
        Map <Integer, Integer []> syndroms = new HashMap <> ();
        int nmk = H.length, n = H [0].length;
        syndroms.put (0, new Integer [n]);
        Arrays.fill (syndroms.get (0), 0);
        
        for (int i = 0; i < n; i++) {
            StringBuilder sb = new StringBuilder ();
            for (int j = 0; j < nmk; j++) {
                sb.append (H [j][i]);
            }
            
            Integer sindrome = Integer.parseInt (sb.toString (), 2);
            syndroms.put (sindrome, new Integer [n]);
            Arrays.fill (syndroms.get (sindrome), 0);
            syndroms.get (sindrome) [i] = 1;
        }
        
        List <Integer> ready = new ArrayList <> (syndroms.keySet ());
        for (int i = 0; i < ready.size (); i++) {
            for (int j = i; j < ready.size (); j++) {
                Integer f = ready.get (i), s = ready.get (j);
                if (syndroms.containsKey (f ^ s)) { continue; }
                
                Integer [] composition = addVector2Vector (syndroms.get (f), 
                                                           syndroms.get (s));
                syndroms.put (f ^ s, composition);
            }
        }
        
        return syndroms;
    }
    
}
