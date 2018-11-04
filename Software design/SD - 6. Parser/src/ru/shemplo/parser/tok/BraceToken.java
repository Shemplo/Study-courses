package ru.shemplo.parser.tok;

public class BraceToken extends AbsToken {

    public BraceToken (String value) {
        super (value);
    }
    
    public char getBrace () {
        return this.value ().charAt (0);
    }
    
}
