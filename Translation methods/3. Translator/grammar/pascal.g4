grammar pascal;

program returns [String code] @init {$code = "";}
//    |------------| |-------------------| |--------------|
	: programHeading programVarDeclaration mainRunningScope {
		$code += $programHeading.header + "\n";
		$code += $programVarDeclaration.declarations + "\n";
		$code += $mainRunningScope.code + "\n";
	  }
	;
	
programHeading returns [String header] @init {$header = "";}
	: PROGRAM identifier SEMI {
		$header += "// translated program " + $identifier.value + "\n";
		$header += "#include <stdio.h>" + "\n";
	  }
	;
	
identifier returns [String value]
	: IDENT {
		$value = $IDENT.text;
	  }
	;
	
programVarDeclaration returns [String declarations] @init {$declarations = "";}
	: (varDeclaration {
		$declarations += $varDeclaration.vars;
	  }
	/*| constDeclaraion {}*/)*
	;
	
varDeclaration returns [String vars] @init {$vars = "";}
	: VAR (varEntryLine {
		$vars += $varEntryLine.line + "\n";
	  })+
	;

varEntryLine returns [String line] @init {$line = "";}
	: varEntries COLON varType SEMI {
		$line += $varType.type + " " + $varEntries.list + ";";
		String [] varNames = $varEntries.list.split (", ");
		for (String var : varNames) {
			VarManager.addVar (var, $varType.type);
		}
	  }
	;
	
varType returns [String type] @init {$type = "void *";}
	: BOOLEAN {$type = "bool";}
	| CHAR    {$type = "char";}
	| INTEGER {$type = "int";}
	| STRING  {$type = "char *";}
	;
	
varEntries returns [String list] @init {$list = "";}
	: identifier {
		$list += $identifier.value;
	  } (COMMA identifier {
	  	$list += ", " + $identifier.value;
	  })*
	|
	;
	
mainRunningScope returns [String code] @init {$code = "";}
	: BEGIN runningScope END DOT {
		$code += "int main () {";
		$code += "\n";
		
		$code += $runningScope.actions;
		$code += "\n";
		
		$code += "    return 0;";
		$code += "\n";
		
		$code += "}";
		$code += "\n";
	  }
	;
	
runningScope returns [String actions] @init {$actions = "";}
	: (runningOperation {
		$actions += $runningOperation.code + "\n";
	  })* 
	;
	
runningOperation returns [String code] @init {$code = "    ";}
	: assignment {
		$code += $assignment.code;
	  }
	| ifExpression {
		$code += $ifExpression.code;
	  }
	| functionCall {
		$code += $functionCall.code;
	  }
	| forExpression {
		$code += $forExpression.code;
	  }
	| whileExpression {
		$code += $whileExpression.code;
	  }
	| repeatExpression {
		$code += $repeatExpression.code;
	  }
	;
	
assignment returns [String code] @init {$code = "";}
	: identifier ASSIGN rightHandExpression SEMI {
		$code = $identifier.value + " = " 
				+ $rightHandExpression.code + ";";
	  }
	;
	
rightHandExpression returns [String code] @init {$code = "";}
	: (
		(LPAREN {$code += "(";})*
	  	(NUM_INT {$code += $NUM_INT.text;} 
	  		| identifier {$code += $identifier.value;}
	  		| functionCall {$code += $functionCall.code;}
	  	)
	  	(RPAREN {$code += ")";})*
	  	(PLUS {$code += " " + $PLUS.text + " ";}
	  		| MINUS {$code += " " + $MINUS.text + " ";} 
	  		| STAR {$code += " " + $STAR.text + " ";}
	  		| SLASH {$code += " " + $SLASH.text + " ";}
	  	)?
	  )+
	| DQUOTE stringExpression? DQUOTE {
		$code += $DQUOTE.text + $stringExpression.string + $DQUOTE.text;
	  }
	;
	
ifExpression returns [String code] @init {$code = "";}
	: IF LPAREN condition RPAREN THEN {
		$code += "if (" + $condition.code + ") {";
		$code += "\n    ";
	  }
	  (BEGIN runningScope END SEMI {
	  		$code += $runningScope.actions;
	  		$code += "}";
	  	}
	  	| runningOperation {
	  		$code += $runningOperation.code;
	  		$code += "\n    ";
	  		$code += "}";
	  	  })
	  (ELSE (BEGIN runningScope END SEMI {
	  				$code += " else {";
	  				$code += "\n    ";
	  				$code += $runningScope.actions;
	  				$code += "}";
	  			}
	  			| runningOperation {
			  		$code += " else {";
	  				$code += "\n    ";
	  				$code += $runningOperation.code;
	  				$code += "\n    ";
	  				$code += "}";
			  	  }
	  			| ifExpression {
	  				$code += " else ";
	  				$code += $ifExpression.code;
	  			  })
	  	| {$code += "\n";}
	  )
	;
	
condition returns [String code] @init {$code = "";}
	: rightHandExpression {
		$code += $rightHandExpression.code;
	  } 
	  (LT {$code += " " + $LT.text + " ";}                  // <
	  	| LE {$code += " " + $LE.text + " ";}               // <=
	  	| GE {$code += " " + $GE.text + " ";}               // >=
	  	| GT {$code += " " + $GT.text + " ";}               // >
	  	| NOT_EQUAL {$code += " " + $NOT_EQUAL.text + " ";} // <>
	  	| AND {$code += " " + $AND.text + " ";}             // and
	  	| OR {$code += " " + $OR.text + " ";}               // or
	  	| EQUAL {$code += " " + $EQUAL.text + " ";}         // =
	  )
	  rightHandExpression {
		$code += $rightHandExpression.code;
	  } 
	;
	
functionCall returns [String code] @init {$code = "";}
	: functionName LPAREN varEntries RPAREN SEMI {
		String [] varNames = $varEntries.list.split (", ");
		if ($functionName.name.equals ("scanf")) {
			String template = VarManager.generateTemplate (varNames);
			$code += "scanf (\"" + template.trim () + "\", ";
			int size = varNames.length;
			for (int i = 0; i < size - 1; i ++) {
				$code += "&" + varNames [i] + ", ";
			}
			$code += "&" + varNames [size - 1] + ");";
			$code += "\n";
		} else if ($functionName.name.equals ("printf")) {
			String template = VarManager.generateTemplate (varNames);
			$code += "printf (\"" + template.trim () + "\\n\", ";
			int size = varNames.length;
			for (int i = 0; i < size - 1; i ++) {
				$code += varNames [i] + ", ";
			}
			$code += varNames [size - 1] + ");";
			$code += "\n";
		} else if ($functionName.name.equals ("div")) {
			$code += varNames [0] + " / " + varNames [1];
		} else if ($functionName.name.equals ("mod")) {
			$code += varNames [0] + " % " + varNames [1];
		} else {
			$code += "// ::function " + $functionName.name + ":: //";
		}
	  }
	;
	
functionName returns [String name] @init {$name = "<unknown>";}
	: READ {$name = "scanf";}
	| WRITE {$name = "printf";}
	| DIV {$name = "div";}
	| MOD {$name = "mod";}
	;
	
forExpression returns [String code] 
	locals [boolean to = true, String varName = "";] @init {$code = "";}
	: FOR identifier ASSIGN rightHandExpression {
		$code += "for (" + $identifier.value + " = " 
					+ $rightHandExpression.code + "; ";
	  	$varName = $identifier.value;
	  }
	  (TO | DOWNTO {$to = false;})
	  rightHandExpression DO {
	  	if ($to) {
	  		$code += $varName + " <= " + $rightHandExpression.code + "; ";
	  		$code += $varName + " ++) {";
	  	} else {
	  		$code += $varName + " >= " + $rightHandExpression.code + "; ";
	  		$code += $varName + " --) {";
	  	}
	  	
	  	$code += "\n    ";
	  }
	  (BEGIN runningScope END SEMI {
	  		$code += $runningScope.actions;
	  		$code += "\n    ";
	  		$code += "}";
	  	}
	  	| runningOperation {
	  		$code += $runningOperation.code;
	  		$code += "\n    ";
	  		$code += "}";
	  	  })
	;

whileExpression returns [String code] @init {$code = "";}
	: WHILE LPAREN condition RPAREN DO {
		$code += "while (" + $condition.code + ") {";
	  	$code += "\n";
	  }
	  (BEGIN runningScope END SEMI {
	  		$code += $runningScope.actions;
	  		$code += "\n";
	  		$code += "}";
	  	}
	  	| runningOperation {
	  		$code += $runningOperation.code;
	  		$code += "\n";
	  		$code += "}";
	  	  })
	;
	
stringExpression returns [String string] @init {$string = "";}
	: (identifier {$string += $identifier.value;}
		| COLON {$string += $COLON.text;}
	  )+
	;
	
repeatExpression returns [String code] @init {$code = "";}
	: REPEAT runningScope UNTIL condition SEMI {
		$code += "do {";
		$code += "\n";
		$code += $runningScope.actions;
		$code += "\n";
		$code += "} while (!(" + $condition.code + "));";
	  }
	;

/*
fragment A : ('a' | 'A');
fragment B : ('b' | 'B');
fragment C : ('c' | 'C');
fragment D : ('d' | 'D');
fragment E : ('e' | 'E');
fragment F : ('f' | 'F');
fragment G : ('g' | 'G');
fragment H : ('h' | 'H');
fragment I : ('i' | 'I');
fragment J : ('j' | 'J');
fragment K : ('k' | 'K');
fragment L : ('l' | 'L');
fragment M : ('m' | 'M');
fragment N : ('n' | 'N');
fragment O : ('o' | 'O');
fragment P : ('p' | 'P');
fragment Q : ('q' | 'Q');
fragment R : ('r' | 'R');
fragment S : ('s' | 'S');
fragment T : ('t' | 'T');
fragment U : ('u' | 'U');
fragment V : ('v' | 'V');
fragment W : ('w' | 'W');
fragment X : ('x' | 'X');
fragment Y : ('y' | 'Y');
fragment Z : ('z' | 'Z');
*/

/*
 * Custom names
 */

ASSIGN : ':=';
SQUOTE : '\'';
LPAREN : '(';
RPAREN : ')';
DQUOTE : '"';
COMMA : ',';
COLON : ':';
SEMI : ';';
DOT : '.';

PLUS : '+';
MINUS : '-';
DIV : 'div';
MOD : 'mod';
STAR : '*';
SLASH : '/';
EQUAL : '=';
NOT_EQUAL: '<>';
LT : '<';
LE : '<=';
GE : '>=';
GT : '>';

PROGRAM : 'program';
BOOLEAN : 'boolean';
INTEGER : 'integer';
STRING : 'string';
REPEAT : 'repeat';
DOWNTO : 'downto';
WHILE : 'while';
BEGIN : 'begin';
UNTIL : 'until';
CHAR : 'char';
THEN : 'then';
ELSE : 'else';
END : 'end';
AND : 'and';
VAR : 'var';
FOR : 'for';
OR : 'or';
IF : 'if';
TO : 'to';
DO : 'do';

READ : 'read';
WRITE : 'write';

WS
   : [ \t\r\n] -> skip
   ;

IDENT
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
   ;
   
NUM_INT
   : ('0' .. '9') + (('.' ('0' .. '9') + (EXPONENT)?)? | EXPONENT)
   ;

fragment EXPONENT
   : ('e') ('+' | '-')? ('0' .. '9') +
   ; 

/*
READ
	: R E A D
	| R E A D L N
	;
	
WRITE
	: W R I T E
	| W R I T E L N
	;
*/
 
/*
 * Default names
 */

/*
AND
   : A N D
   ;


ARRAY
   : A R R A Y
   ;


BEGIN
   : B E G I N
   ;


BOOLEAN
   : B O O L E A N
   ;


CASE
   : C A S E
   ;


CHAR
   : C H A R
   ;


CHR
   : C H R
   ;


CONST
   : C O N S T
   ;


DIV
   : D I V
   ;


DO
   : D O
   ;


DOWNTO
   : D O W N T O
   ;


ELSE
   : E L S E
   ;


END
   : E N D
   ;


FILE
   : F I L E
   ;


FOR
   : F O R
   ;


FUNCTION
   : F U N C T I O N
   ;


GOTO
   : G O T O
   ;


IF
   : I F
   ;


IN
   : I N
   ;


INTEGER
   : I N T E G E R
   ;


LABEL
   : L A B E L
   ;


MOD
   : M O D
   ;


NIL
   : N I L
   ;


NOT
   : N O T
   ;


OF
   : O F
   ;


OR
   : O R
   ;


PACKED
   : P A C K E D
   ;


PROCEDURE
   : P R O C E D U R E
   ;


PROGRAM
   : P R O G R A M
   ;


REAL
   : R E A L
   ;


RECORD
   : R E C O R D
   ;


REPEAT
   : R E P E A T
   ;


SET
   : S E T
   ;


THEN
   : T H E N
   ;


TO
   : T O
   ;


TYPE
   : T Y P E
   ;


UNTIL
   : U N T I L
   ;


VAR
   : V A R
   ;


WHILE
   : W H I L E
   ;


WITH
   : W I T H
   ;


PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


STAR
   : '*'
   ;


SLASH
   : '/'
   ;


ASSIGN
   : ':='
   ;


COMMA
   : ','
   ;


SEMI
   : ';'
   ;


COLON
   : ':'
   ;


EQUAL
   : '='
   ;


NOT_EQUAL
   : '<>'
   ;


LT
   : '<'
   ;


LE
   : '<='
   ;


GE
   : '>='
   ;


GT
   : '>'
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


LBRACK
   : '['
   ;


LBRACK2
   : '(.'
   ;


RBRACK
   : ']'
   ;


RBRACK2
   : '.)'
   ;


POINTER
   : '^'
   ;


AT
   : '@'
   ;


DOT
   : '.'
   ;


DOTDOT
   : '..'
   ;


LCURLY
   : '{'
   ;


RCURLY
   : '}'
   ;


UNIT
   : U N I T
   ;


INTERFACE
   : I N T E R F A C E
   ;


USES
   : U S E S
   ;


STRING
   : S T R I N G
   ;


IMPLEMENTATION
   : I M P L E M E N T A T I O N
   ;


WS
   : [ \t\r\n] -> skip
   ;


COMMENT_1
   : '(*' .*? '*)' -> skip
   ;


COMMENT_2
   : '{' .*? '}' -> skip
   ;


IDENT
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
   ;


STRING_LITERAL
   : '\'' ('\'\'' | ~ ('\''))* '\''
   ;


NUM_INT
   : ('0' .. '9') + (('.' ('0' .. '9') + (EXPONENT)?)? | EXPONENT)
   ;


fragment EXPONENT
   : ('e') ('+' | '-')? ('0' .. '9') +
   ; 
*/