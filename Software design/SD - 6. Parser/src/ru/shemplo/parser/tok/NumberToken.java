package ru.shemplo.parser.tok;


public class NumberToken extends AbsToken {
    
    public NumberToken (String value) {
        super (value);
    }
    
    public int getNumber () {
        return Integer.parseInt (value ());
    }
    
}
