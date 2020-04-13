package ru.shemplo.crypto.lab5;

import java.util.List;
import java.util.stream.Collectors;

public class Tup4 <T1, T2, T3, T4> {
    
    public final T1 T1;
    public final T2 T2;
    public final T3 T3;
    public final T4 T4;
    
    public Tup4 (T1 T1, T2 T2, T3 T3, T4 T4) {
        this.T1 = T1; this.T2 = T2;
        this.T3 = T3; this.T4 = T4;
    }
    
    public String toString () {
        return List.of (T1, T2, T3, T4).stream ().map (String::valueOf).collect (Collectors.joining (", ", "<", ">"));
    }
    
}
