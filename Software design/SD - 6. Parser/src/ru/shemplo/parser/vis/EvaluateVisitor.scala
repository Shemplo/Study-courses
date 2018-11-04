package ru.shemplo.parser.vis
import java.util

import ru.shemplo.parser.tok.{BraceToken, NumberToken, OpToken}

class EvaluateVisitor extends TokenVisitor {
    
    private val stack : util.Stack [Int] = new util.Stack;
    
    override def visit (token: BraceToken): Unit = {
        // No such tokens can be in parsed tree
        throw new UnsupportedOperationException
    }
    
    override def visit (token: NumberToken): Unit = {
        stack push (token getNumber ())
    }
    
    override def visit (token: OpToken): Unit = {
        token.LEFT.accept (this); token.RIGHT.accept (this)
        val right = stack pop (); val left = stack pop ()
        val result : Int = token.getOperation match {
            case "+" => left + right
            case "-" => left - right
            case "*" => left * right
            case "/" => left / right
        }
        
        stack push result
    }
    
    def getValue: Int = stack peek ()
    
}
