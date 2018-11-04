package ru.shemplo.parser.tok.tree

import java.util

import ru.shemplo.parser.tok.{BraceToken, OpToken, Token}
import ru.shemplo.parser.tok.OpToken._

class TreeBuilder (protected val tokens : util.List [Token]) {

    private val maxPriority = getMaxPriority
    private var index : Int = 0
    
    def build (): Token = buildByPriority (0)
    
    private def buildByPriority (priority: Int): Token = {
        val isMax = priority >= maxPriority
        
        var left =
            if (isMax) buildBrace ()
            else       buildByPriority (priority + 1)
        
        left = left match {
            case t : Token => t
            case _         => tokens get index
        }
    
        var loop = true
        while (loop && index + 2 < tokens.size ()) {
            tokens get (index + 1) match {
                case op : OpToken if getPriorityLevel (op) == priority =>
                    index += 2
                    val right = buildByPriority (priority + 1) match {
                        case t: Token => t
                        case _ =>
                            println ("Warning: right part of expression" +
                                     " nof found for " + op.getOperation)
                            tokens get index
                    }
                
                    left = new OpToken (op.getOperation, left, right)
                case _  => loop = false
            }
        }
    
        left
    }
    
    private def buildBrace (): Token = {
        tokens get index match {
            case token : BraceToken if token.getBrace == '(' =>
                index += 1; val exp = buildByPriority (0)
                
                tokens get (index + 1) match {
                    case token: BraceToken if token.getBrace == ')' =>
                        index += 1;
                    case _ =>
                        throw new IllegalStateException ("No closing brace found");
                }
                
                exp
            case _ => null;
        }
    }

}
