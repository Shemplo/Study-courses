package ru.shemplo.parser;

import ru.shemplo.parser.tok.AbsToken;
import ru.shemplo.parser.tok.BraceToken;
import ru.shemplo.parser.tok.Tokenizer;
import ru.shemplo.parser.vis.ParserVisitor;

public class RunParserApp {

    public static void main (String ... args) {
        Tokenizer tokenizer = new Tokenizer ();
        tokenizer.parse ("(2 + 2)");
    }
    
}
