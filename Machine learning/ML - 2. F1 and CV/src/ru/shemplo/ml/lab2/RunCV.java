package ru.shemplo.ml.lab2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;

public class RunCV {

    public static void main (String ... args) throws Exception {
        try (
            Reader r = new InputStreamReader (System.in);
            BufferedReader br = new BufferedReader (r);
                
            PrintWriter pw = new PrintWriter (System.out);
        ) {
            solve (br, pw);
        }
    }
    
    private static int index = 0;
    
    public static void solve (BufferedReader br, PrintWriter pw) throws IOException {
        String line = br.readLine ();
        StringTokenizer st = new StringTokenizer (line);
        
        @SuppressWarnings ("unused")
        int n = Integer.parseInt (st.nextToken ()),
            m = Integer.parseInt (st.nextToken ()),
            k = Integer.parseInt (st.nextToken ());
        
        Map <Integer, List <Integer>> classes = new HashMap <> ();
        st = new StringTokenizer (br.readLine ());
        
        for (int i = 0; i < n; i++) {
            int c = Integer.parseInt (st.nextToken ());
            classes.putIfAbsent (c, new ArrayList <> ());
            classes.get (c).add (i + 1);
        }
        
        List <List <Integer>> parts = new ArrayList <> ();
        for (int i = 0; i < k; i++) {
            List <Integer> part = new ArrayList <> ();
            parts.add (part);
        }
        
        
        classes.values ().forEach (l -> l.forEach (o -> {
            parts.get (index).add (o);
            index = (index + 1) % k;
        }));
        
        parts.forEach (p -> {
            System.out.print (p.size () + " ");
            p.forEach (v -> { System.out.print (v + " "); });
            System.out.println ();
        });
    }
    
}
