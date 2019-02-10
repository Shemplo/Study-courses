package ru.shemplo.profiler;


public class Main {
    
    public static void main (String ... args) {
        for (int i = 0; i < 10; i++) {
            System.out.println ("Iteration: " + i);
        }
    }
    
    public void methodA (String value) {
        System.out.println (value);
    }
    
}
