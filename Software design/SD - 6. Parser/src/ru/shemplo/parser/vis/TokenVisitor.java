package ru.shemplo.parser.vis;

import ru.shemplo.parser.tok.BraceToken;
import ru.shemplo.parser.tok.NumberToken;
import ru.shemplo.parser.tok.OpToken;

public interface TokenVisitor {

    public void visit (BraceToken token);
    
    public void visit (NumberToken token);
    
    public void visit (OpToken token);
    
    
}
