package ru.shemplo.parser.tok

import ru.shemplo.parser.vis.TokenVisitor

abstract class AbsToken (val value : String) extends Token {
    
    protected val VALUE : String = value;
    
    def accept (visitor: TokenVisitor): Unit = {
        try {
            visitor.getClass
                   .getMethod ("visit", this.getClass)
                   .invoke (visitor, this);
        } catch {
            case nsme : NoSuchMethodException => {
                System.err.println (nsme); return;
            };
        }
    }
    
    override def toString (): String = {
        return VALUE;
    };
    
}