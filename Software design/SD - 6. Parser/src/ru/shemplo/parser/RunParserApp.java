package ru.shemplo.parser;

import ru.shemplo.parser.tok.Token;
import ru.shemplo.parser.tok.Tokenizer;
import ru.shemplo.parser.tok.tree.TreeBuilder;
import ru.shemplo.parser.vis.EvaluateVisitor;
import ru.shemplo.parser.vis.TranslatorVisitor;

public class RunParserApp {

    public static void main (String ... args) {
        String expression = "7 + (2 + 12) * 3 - 5343 / 91 / 4";
        System.out.println (expression);
        
        Tokenizer tokenizer = new Tokenizer ();
        tokenizer.parse (expression);
        
        Token root = new TreeBuilder (tokenizer.getTokens ()).build ();
        System.out.println (root);
    
        TranslatorVisitor translator = new TranslatorVisitor ();
        root.accept (translator);
        System.out.println (translator.getCode ());
    
        EvaluateVisitor evaluator = new EvaluateVisitor ();
        root.accept (evaluator);
        System.out.println (evaluator.getValue ());
    }
    
}
