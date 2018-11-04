package ru.shemplo.parser.tok

import java.util

import ru.shemplo.parser.tok.TokenizerState._;

class Tokenizer {
    
    private var buffer : StringBuilder = new StringBuilder;
    private var state  : TokenizerState = null;
    
    private val tokens : util.List [Token] = new util.ArrayList;
    
    def parse (input: String): Unit = {
        input.toCharArray.zipWithIndex.forall (applyNext);
        applyNext (('$', input.length + 1)); // EOI - End Of Input
        println (tokens);
    }
    
    private def applyNext (input : (Char, Int)): Boolean = {
        val (char, index) = input; // unwrapping tuple
        
        val tmpState = char match {
            case alp if IDENTIFIER.test (alp) => IDENTIFIER;
            case num if NUMBER.test (num)     => NUMBER;
            case bra if BRACE.test (bra)      => BRACE;
            case ope if OPERATION.test (ope)  => OPERATION;
            
            case ' ' => return true;
            case '$' => {
                this.flushBuffer;
                return true;
            };
            case _ => null;
        }
        
        if (tmpState == null) {
            val message = "Unexpected character `" + char +
                          "` at index " + index;
            throw  new IllegalStateException (message);
        } else if (!tmpState.equals (state)) {
            this.flushBuffer; // Storing previous token
            this.buffer = new StringBuilder;
            this.state = tmpState;
        }
        
        buffer.append (char);
        return true;
    }
    
    private def flushBuffer (): Unit = {
        if (state != null && buffer.nonEmpty) {
            tokens.add (state.apply (buffer.toString));
        }
    }

}
