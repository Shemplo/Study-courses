package ru.shemplo.parser.vis;

import ru.shemplo.parser.tok.BraceToken;

public class ParserVisitor implements TokenVisitor {

    @Override
    public void visit (BraceToken token) {
        System.out.println (token);
    }
    
}
