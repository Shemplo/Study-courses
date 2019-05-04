package ru.shemplo.infosearch.spellcheck;

import java.util.Objects;
import java.util.function.Function;

public class Trio <F, S, T> {

	public final F F;
	public final S S;
    public final T T;
    
    public Trio (F F, S S, T T) {
        this.F = F; this.S = S;
        this.T = T;
    }
    
    @Override
    public String toString () {
        return "<" + F + "; " + S + "; " + T + ">";
    }
    
    @Override
    public boolean equals (Object obj) {
        if (Objects.isNull (obj) 
            || !(obj instanceof Trio)) { 
            return false; 
        }
        
        if (obj == this) {
        	return true;
        }
        
        Trio <?, ?, ?> trio = (Trio <?, ?, ?>) obj;
        return (F == null ? trio.F == null : F.equals (trio.F))
            && (S == null ? trio.S == null : S.equals (trio.S))
            && (T == null ? trio.T == null : T.equals (trio.T));
    }
    
    public F getF () { return F; }
    public S getS () { return S; }
    public T getT () { return T; }
    
    public <R> Trio <R, S, T> applyF (Function <F, R> converter) {
        return mt (converter.apply (getF ()), getS (), getT ());
    }
    
    public <R> Trio <F, R, T> applyS (Function <S, R> converter) {
        return mt (getF (), converter.apply (getS ()), getT ());
    }
    
    public <R> Trio <F, S, R> applyT (Function <T, R> converter) {
        return mt (getF (), getS (), converter.apply (getT ()));
    }
    
    public static <F, S, T> Trio <F, S, T> mt (F F, S S, T T) {
        return new Trio <> (F, S, T);
    }
	
}
