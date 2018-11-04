package ru.shemplo.parser.vis
import ru.shemplo.parser.tok.{BraceToken, NumberToken, OpToken}

class TranslatorVisitor extends TokenVisitor {
    
    private val buffer : StringBuilder = new StringBuilder
    
    override def visit (token: BraceToken): Unit = {
        // No such tokens can be in parsed tree
        throw new UnsupportedOperationException
    }
    
    override def visit (token: NumberToken): Unit = {
        buffer append (token getNumber ()) append " "
    }
    
    override def visit (token: OpToken): Unit = {
        token.LEFT.accept  (this)
        token.RIGHT.accept (this)
        buffer append token.OPERRATION append " "
    }
    
    def getCode: String = buffer.toString
    
}
