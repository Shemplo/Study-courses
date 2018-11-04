package ru.shemplo.parser;

import ru.shemplo.parser.tok.Token;
import ru.shemplo.parser.tok.Tokenizer;
import ru.shemplo.parser.tok.tree.TreeBuilder;

public class RunParserApp {

    public static void main (String ... args) {
        Tokenizer tokenizer = new Tokenizer ();
        tokenizer.parse ("7 + (2 + 12) * 3 - 5343 / 91 / 4");
        
        Token root = new TreeBuilder (tokenizer.getTokens ()).build ();
        System.out.println (root);
    }
    
}
