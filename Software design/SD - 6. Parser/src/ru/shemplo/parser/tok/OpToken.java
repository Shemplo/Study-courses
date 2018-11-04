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
    
    private static String [][] LEVELS = {
        {"+", "-"},
        {"*", "/"}
    };
    
    public static int getMaxPriority () {
        return LEVELS.length - 1;
    }
    
    public static int getPriorityLevel (OpToken operation) {
        final String value = operation.value ();
        for (int i = 0; i <= getMaxPriority (); i++) {
            for (String oper : LEVELS [i]) {
                if (oper.equals (value)) {
                    return i;
                }
            }
        }
        
        return getMaxPriority () + 1;
    }
    
}
