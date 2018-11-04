package ru.shemplo.parser.tok;

import ru.shemplo.parser.vis.TokenVisitor;

public interface Token {

    public void accept (TokenVisitor visitor);
    
}
