package ru.shemplo.parser.tok.tree

import java.util

import ru.shemplo.parser.tok.{BraceToken, OpToken, Token}
import ru.shemplo.parser.tok.OpToken._

class TreeBuilder (protected val tokens : util.List [Token]) {

    private var index : Int = 0
    
    def build (): Token = buildLow ()
    
    private def buildLow (): Token = {
        var left = buildHigh ()
        
        var loop = true
        while (loop && index + 2 < tokens.size ()) {
            tokens get (index + 1) match {
                case op : OpToken if getPriorityLevel (op) == 0 =>
                    index += 2
                    val right = buildHigh () match {
                        case t: Token => t
                        case _ => tokens get index
                    }
            
                    left = new OpToken (op.getOperation, left, right)
                case _  => loop = false
            }
        }
        
        left
    }
    
    private def buildHigh (): Token = {
        var left = buildBrace () match {
            case t : Token => t
            case _ => tokens get index
        }
        
        var loop = true
        while (loop && index + 2 < tokens.size ()) {
            tokens get (index + 1) match {
                case op : OpToken if getPriorityLevel (op) == 1 =>
                    index += 2
                    val right = buildBrace () match {
                        case t: Token => println (t); t
                        case _ => tokens get index
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
                index += 1; val exp = buildLow ()
                
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
