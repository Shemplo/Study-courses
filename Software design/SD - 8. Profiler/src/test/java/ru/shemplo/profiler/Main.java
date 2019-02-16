package ru.shemplo.profiler;


public class Main {
    
    private static Helper helper = new Helper ();
    
    public static void main (String ... args) {
        for (int i = 4; i < 8; i++) {
            String string = helper.randomString (i);
            printString (string);
            printString (helper.reverse (string));
        }
    }
    
    private static final void printString (String value) {
        System.out.println ("> " + value);
    }
    
}
