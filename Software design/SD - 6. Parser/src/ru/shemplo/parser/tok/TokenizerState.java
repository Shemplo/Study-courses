package ru.shemplo.parser.tok;

import static java.lang.Character.*;

import java.util.function.BiPredicate;
import java.util.function.Function;

public enum TokenizerState implements BiPredicate <TokenizerState, Character>,
                                      Function <String, Token> {
    
    IDENTIFIER ((s, c) -> values () [0].equals (s) && isJavaIdentifierPart (c)
                       || isJavaIdentifierStart (c),
                null),
    
    NUMBER     ((s, c) -> isDigit (c),
                NumberToken::new),
    
    BRACE      ((s, c) -> c.equals ('(') || c.equals (')'),
                BraceToken::new),
    
    OPERATION  ((s, c) -> c.equals ('+') || c.equals ('-')
                || c.equals ('*') || c.equals ('/'),
                OpToken::new);
    
    private final BiPredicate <TokenizerState, Character> P;
    private final Function <String, Token> F;
    
    private TokenizerState (BiPredicate <TokenizerState, Character> predicate,
                            Function <String, Token> function) {
        this.P = predicate;
        this.F = function;
    }

    @Override
    public boolean test (TokenizerState state, Character c) {
        return P.test (state, c);
    }

    @Override
    public Token apply (String value) {
        return F.apply (value);
    }
    
}
