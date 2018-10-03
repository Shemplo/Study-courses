grammar Grammar;

@header{
package ru.shemplo.mt.task4.antlr;

import ru.shemplo.mt.task4.CodeGenerator.Repeat;
}

grammar_file 
    : grammar_rule+ 
      grammar_declaration*
    ;
    
grammar_rule
    : ident local_variables? LCBRACKET grammar_rule_entry+ RCBARCKET
    ;

local_variables
    : LPAREN local_variable* RPAREN;
    
local_variable
    : ident code_expression COMMA?;
    
grammar_rule_entry 
    returns [Repeat repeat] 
    @init { $repeat = Repeat.SINGLE; }
    : ident 
      ( PLUS  { $repeat = Repeat.ONE_OR_MORE; } 
      | QMARK { $repeat = Repeat.ZERO_OR_ONE; }
      | STAR  { $repeat = Repeat.ZERO_OR_MORE; }
      )? 
      grammar_rule_operaion? VLINE?;
    
grammar_rule_operaion
    : LCBRACKET code_expression* RCBARCKET
    ;

code_expression
    : ident EQUAL operation_unit code_operation? SEMI?
    ;
    
code_operation
    : PLUS operation_unit code_operation?
    ;
    
operation_unit
    : expand_ident
    | DQUOTE 
      (expand_ident | SLASH | BSLASH | STAR | LPAREN | RPAREN
       | LCBRACKET | RCBARCKET | SEMI | NUMBER | EQUAL | PLUS
      )* 
      DQUOTE
    ;
    
grammar_declaration
    : ident COLON 
      (expand_ident | regular_expression | character) 
      SEMI
    ;
    
ident
    : IDENT
    ;

expand_ident
    : DOLL? IDENT (IDENT | DOT | '_')*
    ;
    
regular_expression
    :  SLASH 
       (IDENT | LPAREN | RPAREN | LSBRACKET | RSBRACKET
        | DOT | STAR | PLUS | QMARK | BSLASH | '_' | '-'
        | COMMA | COLON | EQUAL | NUMBER
       )+
       SLASH
    ;
    
character
    : '\'' . '\''
    ;

IDENT
    : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
    ;
    
NUMBER
    : ('0' .. '9')+
    ;
    
WS
    : [ \t\r\n] -> skip
    ;

LSBRACKET : '[';
RSBRACKET : ']';
LCBRACKET : '{';
RCBARCKET : '}';
ASSIGN : ':=';
BSLASH : '\\';
SQUOTE : '\'';
LPAREN : '(';
RPAREN : ')';
DQUOTE : '"';
COMMA : ',';
COLON : ':';
EQUAL : '=';
QMARK : '?';
SLASH : '/';
VLINE : '|';
DOLL : '$';
PLUS : '+';
SEMI : ';';
STAR : '*';
DOT : '.';
