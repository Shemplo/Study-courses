package ru.shemplo.ml.lab2;

import java.util.StringTokenizer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;

public class RunF1 {

    public static void main (String ... args) throws Exception {
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
                
            PrintWriter pw = new PrintWriter (System.out);
        ) {
            solve (br, pw);
        }
    }
    
    public static void solve (BufferedReader br, PrintWriter pw) throws IOException {
        String line = br.readLine ().trim ();
        int classes = Integer.parseInt (line);
        
        double [][] matrix = new double [classes][classes];
        double [] columns = new double [classes],
                  lines   = new double [classes];
        double total = 0;
        
        for (int i = 0; i < classes; i++) {
            StringTokenizer st = new StringTokenizer (br.readLine ().trim ());
            for (int j = 0; j < classes; j++) {
                matrix [i][j] = Double.parseDouble (st.nextToken ());
                
                columns [j] += matrix [i][j];
                lines [i] += matrix [i][j];
                total += matrix [i][j];
            }
        }
        
        double sumP = 0, sumR = 0, score = 0;
        for (int i = 0; i < classes; i++) {
            double tp = matrix [i][i];
            double precision = (tp == 0) ? 0 : (tp / columns [i]),
                   recall = (tp == 0) ? 0 : (tp / lines [i]);
            score += f1 (precision, recall) * lines [i];
            sumP += precision * lines [i];
            sumR += recall * lines [i];
        }
        
        System.out.println (f1 (sumP / total, sumR / total));
        System.out.println (score / total);
    }
    
    public static double f1 (double p, double r) {
        if (Math.abs (p + r) >= 1e-4) {
            return 2 * (p * r) / (p + r);
        }
        
        return 0.0d;
    }
    
}
