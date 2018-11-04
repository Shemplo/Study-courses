package ru.shemplo.parser;

import ru.shemplo.parser.tok.AbsToken;
import ru.shemplo.parser.tok.BraceToken;
import ru.shemplo.parser.tok.Token;
import ru.shemplo.parser.tok.Tokenizer;
import ru.shemplo.parser.tok.tree.TreeBuilder;
import ru.shemplo.parser.vis.ParserVisitor;

public class RunParserApp {

    public static void main (String ... args) {
        Tokenizer tokenizer = new Tokenizer ();
        tokenizer.parse ("7 + (2 + 2) * 3 - 3 / 9");
        
        Token root = new TreeBuilder (tokenizer.getTokens ()).build ();
        System.out.println (root);
    }
    
}
