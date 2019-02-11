package ru.shemplo.profiler;


public class Main {
    
    public static void main (String ... args) {
        for (int i = 0; i < 10; i++) {
            methodA ("Iteration: " + i, i, (long) i, i % 2 == 0, (double) i, (float) i);
        }
    }
    
    public static void methodA (String value, int a, long b, boolean c, double d, float f) {
        System.out.println (value + " " + a + " " + b + " " + c + " " + d + " " + f);
    }
    
}
