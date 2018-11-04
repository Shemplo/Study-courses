package ru.shemplo.parser.tok

import ru.shemplo.parser.vis.TokenVisitor

abstract class AbsToken (protected val value : String) extends Token {
    
    def accept (visitor: TokenVisitor): Unit = {
        try {
            visitor.getClass
                   .getMethod ("visit", this.getClass)
                   .invoke (visitor, this)
        } catch {
            case nsme : NoSuchMethodException =>
                System.err.println (nsme)
        }
    }
    
    override def toString: String = value
    
}