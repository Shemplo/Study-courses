package ru.shemplo.parser.tok;


public class OpToken extends AbsToken {

    public OpToken (String value) {
        super (value);
    }
    
    public OpToken (String value, Token left, Token right) {
        super ("{" + left + ", " + value + ", " + right + "}");
    }
    
    public String getOperation () {
        return value ();
    }
    
}
