package ru.shemplo.parser.tok;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

public enum TokenizerState implements Predicate <Character>, Function <String, Token> {
    
    IDENTIFIER (Character::isLetter,
                null),
    
    NUMBER     (Character::isDigit,
                NumberToken::new),
    
    BRACE      (c -> c.equals ('(') || c.equals (')'),
                BraceToken::new),
    
    OPERATION  (c -> c.equals ('+') || c.equals ('-')
                || c.equals ('*') || c.equals ('/'),
                OpToken::new);
    
    private final Function <String, Token> F;
    private final Predicate <Character> P;
    
    private TokenizerState (Predicate <Character> predicate,
                            Function <String, Token> function) {
        this.P = predicate;
        this.F = function;
    }

    @Override
    public boolean test (Character c) {
        return P.test (c);
    }

    @Override
    public Token apply (String value) {
        return F.apply (value);
    }
    
}
