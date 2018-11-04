package ru.shemplo.parser.vis;

import ru.shemplo.parser.tok.BraceToken;

public interface TokenVisitor {

    public void visit (BraceToken token);
    
}
