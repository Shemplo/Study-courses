// Generated from pascal.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class pascalParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ASSIGN=1, SQUOTE=2, LPAREN=3, RPAREN=4, DQUOTE=5, COMMA=6, COLON=7, SEMI=8, 
		DOT=9, PLUS=10, MINUS=11, DIV=12, MOD=13, STAR=14, SLASH=15, EQUAL=16, 
		NOT_EQUAL=17, LT=18, LE=19, GE=20, GT=21, PROGRAM=22, BOOLEAN=23, INTEGER=24, 
		STRING=25, REPEAT=26, DOWNTO=27, WHILE=28, BEGIN=29, UNTIL=30, CHAR=31, 
		THEN=32, ELSE=33, END=34, AND=35, VAR=36, FOR=37, OR=38, IF=39, TO=40, 
		DO=41, READ=42, WRITE=43, WS=44, IDENT=45, NUM_INT=46;
	public static final int
		RULE_program = 0, RULE_programHeading = 1, RULE_identifier = 2, RULE_programVarDeclaration = 3, 
		RULE_varDeclaration = 4, RULE_varEntryLine = 5, RULE_varType = 6, RULE_varEntries = 7, 
		RULE_mainRunningScope = 8, RULE_runningScope = 9, RULE_runningOperation = 10, 
		RULE_assignment = 11, RULE_rightHandExpression = 12, RULE_ifExpression = 13, 
		RULE_condition = 14, RULE_functionCall = 15, RULE_functionName = 16, RULE_forExpression = 17, 
		RULE_whileExpression = 18, RULE_stringExpression = 19, RULE_repeatExpression = 20;
	public static final String[] ruleNames = {
		"program", "programHeading", "identifier", "programVarDeclaration", "varDeclaration", 
		"varEntryLine", "varType", "varEntries", "mainRunningScope", "runningScope", 
		"runningOperation", "assignment", "rightHandExpression", "ifExpression", 
		"condition", "functionCall", "functionName", "forExpression", "whileExpression", 
		"stringExpression", "repeatExpression"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "':='", "'''", "'('", "')'", "'\"'", "','", "':'", "';'", "'.'", 
		"'+'", "'-'", "'div'", "'mod'", "'*'", "'/'", "'='", "'<>'", "'<'", "'<='", 
		"'>='", "'>'", "'program'", "'boolean'", "'integer'", "'string'", "'repeat'", 
		"'downto'", "'while'", "'begin'", "'until'", "'char'", "'then'", "'else'", 
		"'end'", "'and'", "'var'", "'for'", "'or'", "'if'", "'to'", "'do'", "'read'", 
		"'write'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "ASSIGN", "SQUOTE", "LPAREN", "RPAREN", "DQUOTE", "COMMA", "COLON", 
		"SEMI", "DOT", "PLUS", "MINUS", "DIV", "MOD", "STAR", "SLASH", "EQUAL", 
		"NOT_EQUAL", "LT", "LE", "GE", "GT", "PROGRAM", "BOOLEAN", "INTEGER", 
		"STRING", "REPEAT", "DOWNTO", "WHILE", "BEGIN", "UNTIL", "CHAR", "THEN", 
		"ELSE", "END", "AND", "VAR", "FOR", "OR", "IF", "TO", "DO", "READ", "WRITE", 
		"WS", "IDENT", "NUM_INT"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "pascal.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public pascalParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class ProgramContext extends ParserRuleContext {
		public String code;
		public ProgramHeadingContext programHeading;
		public ProgramVarDeclarationContext programVarDeclaration;
		public MainRunningScopeContext mainRunningScope;
		public ProgramHeadingContext programHeading() {
			return getRuleContext(ProgramHeadingContext.class,0);
		}
		public ProgramVarDeclarationContext programVarDeclaration() {
			return getRuleContext(ProgramVarDeclarationContext.class,0);
		}
		public MainRunningScopeContext mainRunningScope() {
			return getRuleContext(MainRunningScopeContext.class,0);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitProgram(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		((ProgramContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(42);
			((ProgramContext)_localctx).programHeading = programHeading();
			setState(43);
			((ProgramContext)_localctx).programVarDeclaration = programVarDeclaration();
			setState(44);
			((ProgramContext)_localctx).mainRunningScope = mainRunningScope();

					_localctx.code += ((ProgramContext)_localctx).programHeading.header + "\n";
					_localctx.code += ((ProgramContext)_localctx).programVarDeclaration.declarations + "\n";
					_localctx.code += ((ProgramContext)_localctx).mainRunningScope.code + "\n";
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ProgramHeadingContext extends ParserRuleContext {
		public String header;
		public IdentifierContext identifier;
		public TerminalNode PROGRAM() { return getToken(pascalParser.PROGRAM, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(pascalParser.SEMI, 0); }
		public ProgramHeadingContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_programHeading; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterProgramHeading(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitProgramHeading(this);
		}
	}

	public final ProgramHeadingContext programHeading() throws RecognitionException {
		ProgramHeadingContext _localctx = new ProgramHeadingContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_programHeading);
		((ProgramHeadingContext)_localctx).header =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(47);
			match(PROGRAM);
			setState(48);
			((ProgramHeadingContext)_localctx).identifier = identifier();
			setState(49);
			match(SEMI);

					_localctx.header += "// translated program " + ((ProgramHeadingContext)_localctx).identifier.value + "\n";
					_localctx.header += "#include <stdio.h>" + "\n";
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IdentifierContext extends ParserRuleContext {
		public String value;
		public Token IDENT;
		public TerminalNode IDENT() { return getToken(pascalParser.IDENT, 0); }
		public IdentifierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_identifier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterIdentifier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitIdentifier(this);
		}
	}

	public final IdentifierContext identifier() throws RecognitionException {
		IdentifierContext _localctx = new IdentifierContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_identifier);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(52);
			((IdentifierContext)_localctx).IDENT = match(IDENT);

					((IdentifierContext)_localctx).value =  (((IdentifierContext)_localctx).IDENT!=null?((IdentifierContext)_localctx).IDENT.getText():null);
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ProgramVarDeclarationContext extends ParserRuleContext {
		public String declarations;
		public VarDeclarationContext varDeclaration;
		public List<VarDeclarationContext> varDeclaration() {
			return getRuleContexts(VarDeclarationContext.class);
		}
		public VarDeclarationContext varDeclaration(int i) {
			return getRuleContext(VarDeclarationContext.class,i);
		}
		public ProgramVarDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_programVarDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterProgramVarDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitProgramVarDeclaration(this);
		}
	}

	public final ProgramVarDeclarationContext programVarDeclaration() throws RecognitionException {
		ProgramVarDeclarationContext _localctx = new ProgramVarDeclarationContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_programVarDeclaration);
		((ProgramVarDeclarationContext)_localctx).declarations =  "";
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(60);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==VAR) {
				{
				{
				setState(55);
				((ProgramVarDeclarationContext)_localctx).varDeclaration = varDeclaration();

						_localctx.declarations += ((ProgramVarDeclarationContext)_localctx).varDeclaration.vars;
					  
				}
				}
				setState(62);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarDeclarationContext extends ParserRuleContext {
		public String vars;
		public VarEntryLineContext varEntryLine;
		public TerminalNode VAR() { return getToken(pascalParser.VAR, 0); }
		public List<VarEntryLineContext> varEntryLine() {
			return getRuleContexts(VarEntryLineContext.class);
		}
		public VarEntryLineContext varEntryLine(int i) {
			return getRuleContext(VarEntryLineContext.class,i);
		}
		public VarDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterVarDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitVarDeclaration(this);
		}
	}

	public final VarDeclarationContext varDeclaration() throws RecognitionException {
		VarDeclarationContext _localctx = new VarDeclarationContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_varDeclaration);
		((VarDeclarationContext)_localctx).vars =  "";
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(63);
			match(VAR);
			setState(67); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(64);
				((VarDeclarationContext)_localctx).varEntryLine = varEntryLine();

						_localctx.vars += ((VarDeclarationContext)_localctx).varEntryLine.line + "\n";
					  
				}
				}
				setState(69); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==COLON || _la==IDENT );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarEntryLineContext extends ParserRuleContext {
		public String line;
		public VarEntriesContext varEntries;
		public VarTypeContext varType;
		public VarEntriesContext varEntries() {
			return getRuleContext(VarEntriesContext.class,0);
		}
		public TerminalNode COLON() { return getToken(pascalParser.COLON, 0); }
		public VarTypeContext varType() {
			return getRuleContext(VarTypeContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(pascalParser.SEMI, 0); }
		public VarEntryLineContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varEntryLine; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterVarEntryLine(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitVarEntryLine(this);
		}
	}

	public final VarEntryLineContext varEntryLine() throws RecognitionException {
		VarEntryLineContext _localctx = new VarEntryLineContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_varEntryLine);
		((VarEntryLineContext)_localctx).line =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(71);
			((VarEntryLineContext)_localctx).varEntries = varEntries();
			setState(72);
			match(COLON);
			setState(73);
			((VarEntryLineContext)_localctx).varType = varType();
			setState(74);
			match(SEMI);

					_localctx.line += ((VarEntryLineContext)_localctx).varType.type + " " + ((VarEntryLineContext)_localctx).varEntries.list + ";";
					String [] varNames = ((VarEntryLineContext)_localctx).varEntries.list.split (", ");
					for (String var : varNames) {
						VarManager.addVar (var, ((VarEntryLineContext)_localctx).varType.type);
					}
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarTypeContext extends ParserRuleContext {
		public String type;
		public TerminalNode BOOLEAN() { return getToken(pascalParser.BOOLEAN, 0); }
		public TerminalNode CHAR() { return getToken(pascalParser.CHAR, 0); }
		public TerminalNode INTEGER() { return getToken(pascalParser.INTEGER, 0); }
		public TerminalNode STRING() { return getToken(pascalParser.STRING, 0); }
		public VarTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterVarType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitVarType(this);
		}
	}

	public final VarTypeContext varType() throws RecognitionException {
		VarTypeContext _localctx = new VarTypeContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_varType);
		((VarTypeContext)_localctx).type =  "void *";
		try {
			setState(85);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case BOOLEAN:
				enterOuterAlt(_localctx, 1);
				{
				setState(77);
				match(BOOLEAN);
				((VarTypeContext)_localctx).type =  "bool";
				}
				break;
			case CHAR:
				enterOuterAlt(_localctx, 2);
				{
				setState(79);
				match(CHAR);
				((VarTypeContext)_localctx).type =  "char";
				}
				break;
			case INTEGER:
				enterOuterAlt(_localctx, 3);
				{
				setState(81);
				match(INTEGER);
				((VarTypeContext)_localctx).type =  "int";
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 4);
				{
				setState(83);
				match(STRING);
				((VarTypeContext)_localctx).type =  "char *";
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarEntriesContext extends ParserRuleContext {
		public String list;
		public IdentifierContext identifier;
		public List<IdentifierContext> identifier() {
			return getRuleContexts(IdentifierContext.class);
		}
		public IdentifierContext identifier(int i) {
			return getRuleContext(IdentifierContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(pascalParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(pascalParser.COMMA, i);
		}
		public VarEntriesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varEntries; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterVarEntries(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitVarEntries(this);
		}
	}

	public final VarEntriesContext varEntries() throws RecognitionException {
		VarEntriesContext _localctx = new VarEntriesContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_varEntries);
		((VarEntriesContext)_localctx).list =  "";
		int _la;
		try {
			setState(99);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENT:
				enterOuterAlt(_localctx, 1);
				{
				setState(87);
				((VarEntriesContext)_localctx).identifier = identifier();

						_localctx.list += ((VarEntriesContext)_localctx).identifier.value;
					  
				setState(95);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(89);
					match(COMMA);
					setState(90);
					((VarEntriesContext)_localctx).identifier = identifier();

						  	_localctx.list += ", " + ((VarEntriesContext)_localctx).identifier.value;
						  
					}
					}
					setState(97);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				break;
			case RPAREN:
			case COLON:
				enterOuterAlt(_localctx, 2);
				{
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MainRunningScopeContext extends ParserRuleContext {
		public String code;
		public RunningScopeContext runningScope;
		public TerminalNode BEGIN() { return getToken(pascalParser.BEGIN, 0); }
		public RunningScopeContext runningScope() {
			return getRuleContext(RunningScopeContext.class,0);
		}
		public TerminalNode END() { return getToken(pascalParser.END, 0); }
		public TerminalNode DOT() { return getToken(pascalParser.DOT, 0); }
		public MainRunningScopeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_mainRunningScope; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterMainRunningScope(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitMainRunningScope(this);
		}
	}

	public final MainRunningScopeContext mainRunningScope() throws RecognitionException {
		MainRunningScopeContext _localctx = new MainRunningScopeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_mainRunningScope);
		((MainRunningScopeContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(101);
			match(BEGIN);
			setState(102);
			((MainRunningScopeContext)_localctx).runningScope = runningScope();
			setState(103);
			match(END);
			setState(104);
			match(DOT);

					_localctx.code += "int main () {";
					_localctx.code += "\n";
					
					_localctx.code += ((MainRunningScopeContext)_localctx).runningScope.actions;
					_localctx.code += "\n";
					
					_localctx.code += "    return 0;";
					_localctx.code += "\n";
					
					_localctx.code += "}";
					_localctx.code += "\n";
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RunningScopeContext extends ParserRuleContext {
		public String actions;
		public RunningOperationContext runningOperation;
		public List<RunningOperationContext> runningOperation() {
			return getRuleContexts(RunningOperationContext.class);
		}
		public RunningOperationContext runningOperation(int i) {
			return getRuleContext(RunningOperationContext.class,i);
		}
		public RunningScopeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_runningScope; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterRunningScope(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitRunningScope(this);
		}
	}

	public final RunningScopeContext runningScope() throws RecognitionException {
		RunningScopeContext _localctx = new RunningScopeContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_runningScope);
		((RunningScopeContext)_localctx).actions =  "";
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(112);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << DIV) | (1L << MOD) | (1L << REPEAT) | (1L << WHILE) | (1L << FOR) | (1L << IF) | (1L << READ) | (1L << WRITE) | (1L << IDENT))) != 0)) {
				{
				{
				setState(107);
				((RunningScopeContext)_localctx).runningOperation = runningOperation();

						_localctx.actions += ((RunningScopeContext)_localctx).runningOperation.code + "\n";
					  
				}
				}
				setState(114);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RunningOperationContext extends ParserRuleContext {
		public String code;
		public AssignmentContext assignment;
		public IfExpressionContext ifExpression;
		public FunctionCallContext functionCall;
		public ForExpressionContext forExpression;
		public WhileExpressionContext whileExpression;
		public RepeatExpressionContext repeatExpression;
		public AssignmentContext assignment() {
			return getRuleContext(AssignmentContext.class,0);
		}
		public IfExpressionContext ifExpression() {
			return getRuleContext(IfExpressionContext.class,0);
		}
		public FunctionCallContext functionCall() {
			return getRuleContext(FunctionCallContext.class,0);
		}
		public ForExpressionContext forExpression() {
			return getRuleContext(ForExpressionContext.class,0);
		}
		public WhileExpressionContext whileExpression() {
			return getRuleContext(WhileExpressionContext.class,0);
		}
		public RepeatExpressionContext repeatExpression() {
			return getRuleContext(RepeatExpressionContext.class,0);
		}
		public RunningOperationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_runningOperation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterRunningOperation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitRunningOperation(this);
		}
	}

	public final RunningOperationContext runningOperation() throws RecognitionException {
		RunningOperationContext _localctx = new RunningOperationContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_runningOperation);
		((RunningOperationContext)_localctx).code =  "    ";
		try {
			setState(133);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENT:
				enterOuterAlt(_localctx, 1);
				{
				setState(115);
				((RunningOperationContext)_localctx).assignment = assignment();

						_localctx.code += ((RunningOperationContext)_localctx).assignment.code;
					  
				}
				break;
			case IF:
				enterOuterAlt(_localctx, 2);
				{
				setState(118);
				((RunningOperationContext)_localctx).ifExpression = ifExpression();

						_localctx.code += ((RunningOperationContext)_localctx).ifExpression.code;
					  
				}
				break;
			case DIV:
			case MOD:
			case READ:
			case WRITE:
				enterOuterAlt(_localctx, 3);
				{
				setState(121);
				((RunningOperationContext)_localctx).functionCall = functionCall();

						_localctx.code += ((RunningOperationContext)_localctx).functionCall.code;
					  
				}
				break;
			case FOR:
				enterOuterAlt(_localctx, 4);
				{
				setState(124);
				((RunningOperationContext)_localctx).forExpression = forExpression();

						_localctx.code += ((RunningOperationContext)_localctx).forExpression.code;
					  
				}
				break;
			case WHILE:
				enterOuterAlt(_localctx, 5);
				{
				setState(127);
				((RunningOperationContext)_localctx).whileExpression = whileExpression();

						_localctx.code += ((RunningOperationContext)_localctx).whileExpression.code;
					  
				}
				break;
			case REPEAT:
				enterOuterAlt(_localctx, 6);
				{
				setState(130);
				((RunningOperationContext)_localctx).repeatExpression = repeatExpression();

						_localctx.code += ((RunningOperationContext)_localctx).repeatExpression.code;
					  
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AssignmentContext extends ParserRuleContext {
		public String code;
		public IdentifierContext identifier;
		public RightHandExpressionContext rightHandExpression;
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode ASSIGN() { return getToken(pascalParser.ASSIGN, 0); }
		public RightHandExpressionContext rightHandExpression() {
			return getRuleContext(RightHandExpressionContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(pascalParser.SEMI, 0); }
		public AssignmentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assignment; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterAssignment(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitAssignment(this);
		}
	}

	public final AssignmentContext assignment() throws RecognitionException {
		AssignmentContext _localctx = new AssignmentContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_assignment);
		((AssignmentContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(135);
			((AssignmentContext)_localctx).identifier = identifier();
			setState(136);
			match(ASSIGN);
			setState(137);
			((AssignmentContext)_localctx).rightHandExpression = rightHandExpression();
			setState(138);
			match(SEMI);

					((AssignmentContext)_localctx).code =  ((AssignmentContext)_localctx).identifier.value + " = " 
							+ ((AssignmentContext)_localctx).rightHandExpression.code + ";";
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RightHandExpressionContext extends ParserRuleContext {
		public String code;
		public Token NUM_INT;
		public IdentifierContext identifier;
		public FunctionCallContext functionCall;
		public Token PLUS;
		public Token MINUS;
		public Token STAR;
		public Token SLASH;
		public Token DQUOTE;
		public StringExpressionContext stringExpression;
		public List<TerminalNode> NUM_INT() { return getTokens(pascalParser.NUM_INT); }
		public TerminalNode NUM_INT(int i) {
			return getToken(pascalParser.NUM_INT, i);
		}
		public List<IdentifierContext> identifier() {
			return getRuleContexts(IdentifierContext.class);
		}
		public IdentifierContext identifier(int i) {
			return getRuleContext(IdentifierContext.class,i);
		}
		public List<FunctionCallContext> functionCall() {
			return getRuleContexts(FunctionCallContext.class);
		}
		public FunctionCallContext functionCall(int i) {
			return getRuleContext(FunctionCallContext.class,i);
		}
		public List<TerminalNode> LPAREN() { return getTokens(pascalParser.LPAREN); }
		public TerminalNode LPAREN(int i) {
			return getToken(pascalParser.LPAREN, i);
		}
		public List<TerminalNode> RPAREN() { return getTokens(pascalParser.RPAREN); }
		public TerminalNode RPAREN(int i) {
			return getToken(pascalParser.RPAREN, i);
		}
		public List<TerminalNode> PLUS() { return getTokens(pascalParser.PLUS); }
		public TerminalNode PLUS(int i) {
			return getToken(pascalParser.PLUS, i);
		}
		public List<TerminalNode> MINUS() { return getTokens(pascalParser.MINUS); }
		public TerminalNode MINUS(int i) {
			return getToken(pascalParser.MINUS, i);
		}
		public List<TerminalNode> STAR() { return getTokens(pascalParser.STAR); }
		public TerminalNode STAR(int i) {
			return getToken(pascalParser.STAR, i);
		}
		public List<TerminalNode> SLASH() { return getTokens(pascalParser.SLASH); }
		public TerminalNode SLASH(int i) {
			return getToken(pascalParser.SLASH, i);
		}
		public List<TerminalNode> DQUOTE() { return getTokens(pascalParser.DQUOTE); }
		public TerminalNode DQUOTE(int i) {
			return getToken(pascalParser.DQUOTE, i);
		}
		public StringExpressionContext stringExpression() {
			return getRuleContext(StringExpressionContext.class,0);
		}
		public RightHandExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rightHandExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterRightHandExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitRightHandExpression(this);
		}
	}

	public final RightHandExpressionContext rightHandExpression() throws RecognitionException {
		RightHandExpressionContext _localctx = new RightHandExpressionContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_rightHandExpression);
		((RightHandExpressionContext)_localctx).code =  "";
		int _la;
		try {
			int _alt;
			setState(185);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LPAREN:
			case DIV:
			case MOD:
			case READ:
			case WRITE:
			case IDENT:
			case NUM_INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(175); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(145);
					_errHandler.sync(this);
					_la = _input.LA(1);
					while (_la==LPAREN) {
						{
						{
						setState(141);
						match(LPAREN);
						_localctx.code += "(";
						}
						}
						setState(147);
						_errHandler.sync(this);
						_la = _input.LA(1);
					}
					setState(156);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case NUM_INT:
						{
						setState(148);
						((RightHandExpressionContext)_localctx).NUM_INT = match(NUM_INT);
						_localctx.code += (((RightHandExpressionContext)_localctx).NUM_INT!=null?((RightHandExpressionContext)_localctx).NUM_INT.getText():null);
						}
						break;
					case IDENT:
						{
						setState(150);
						((RightHandExpressionContext)_localctx).identifier = identifier();
						_localctx.code += ((RightHandExpressionContext)_localctx).identifier.value;
						}
						break;
					case DIV:
					case MOD:
					case READ:
					case WRITE:
						{
						setState(153);
						((RightHandExpressionContext)_localctx).functionCall = functionCall();
						_localctx.code += ((RightHandExpressionContext)_localctx).functionCall.code;
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(162);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
					while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
						if ( _alt==1 ) {
							{
							{
							setState(158);
							match(RPAREN);
							_localctx.code += ")";
							}
							} 
						}
						setState(164);
						_errHandler.sync(this);
						_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
					}
					setState(173);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case PLUS:
						{
						setState(165);
						((RightHandExpressionContext)_localctx).PLUS = match(PLUS);
						_localctx.code += " " + (((RightHandExpressionContext)_localctx).PLUS!=null?((RightHandExpressionContext)_localctx).PLUS.getText():null) + " ";
						}
						break;
					case MINUS:
						{
						setState(167);
						((RightHandExpressionContext)_localctx).MINUS = match(MINUS);
						_localctx.code += " " + (((RightHandExpressionContext)_localctx).MINUS!=null?((RightHandExpressionContext)_localctx).MINUS.getText():null) + " ";
						}
						break;
					case STAR:
						{
						setState(169);
						((RightHandExpressionContext)_localctx).STAR = match(STAR);
						_localctx.code += " " + (((RightHandExpressionContext)_localctx).STAR!=null?((RightHandExpressionContext)_localctx).STAR.getText():null) + " ";
						}
						break;
					case SLASH:
						{
						setState(171);
						((RightHandExpressionContext)_localctx).SLASH = match(SLASH);
						_localctx.code += " " + (((RightHandExpressionContext)_localctx).SLASH!=null?((RightHandExpressionContext)_localctx).SLASH.getText():null) + " ";
						}
						break;
					case LPAREN:
					case RPAREN:
					case SEMI:
					case DIV:
					case MOD:
					case EQUAL:
					case NOT_EQUAL:
					case LT:
					case LE:
					case GE:
					case GT:
					case DOWNTO:
					case AND:
					case OR:
					case TO:
					case DO:
					case READ:
					case WRITE:
					case IDENT:
					case NUM_INT:
						break;
					default:
						break;
					}
					}
					}
					setState(177); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << LPAREN) | (1L << DIV) | (1L << MOD) | (1L << READ) | (1L << WRITE) | (1L << IDENT) | (1L << NUM_INT))) != 0) );
				}
				break;
			case DQUOTE:
				enterOuterAlt(_localctx, 2);
				{
				setState(179);
				((RightHandExpressionContext)_localctx).DQUOTE = match(DQUOTE);
				setState(181);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==COLON || _la==IDENT) {
					{
					setState(180);
					((RightHandExpressionContext)_localctx).stringExpression = stringExpression();
					}
				}

				setState(183);
				((RightHandExpressionContext)_localctx).DQUOTE = match(DQUOTE);

						_localctx.code += (((RightHandExpressionContext)_localctx).DQUOTE!=null?((RightHandExpressionContext)_localctx).DQUOTE.getText():null) + ((RightHandExpressionContext)_localctx).stringExpression.string + (((RightHandExpressionContext)_localctx).DQUOTE!=null?((RightHandExpressionContext)_localctx).DQUOTE.getText():null);
					  
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IfExpressionContext extends ParserRuleContext {
		public String code;
		public ConditionContext condition;
		public RunningScopeContext runningScope;
		public RunningOperationContext runningOperation;
		public IfExpressionContext ifExpression;
		public TerminalNode IF() { return getToken(pascalParser.IF, 0); }
		public TerminalNode LPAREN() { return getToken(pascalParser.LPAREN, 0); }
		public ConditionContext condition() {
			return getRuleContext(ConditionContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(pascalParser.RPAREN, 0); }
		public TerminalNode THEN() { return getToken(pascalParser.THEN, 0); }
		public List<TerminalNode> BEGIN() { return getTokens(pascalParser.BEGIN); }
		public TerminalNode BEGIN(int i) {
			return getToken(pascalParser.BEGIN, i);
		}
		public List<RunningScopeContext> runningScope() {
			return getRuleContexts(RunningScopeContext.class);
		}
		public RunningScopeContext runningScope(int i) {
			return getRuleContext(RunningScopeContext.class,i);
		}
		public List<TerminalNode> END() { return getTokens(pascalParser.END); }
		public TerminalNode END(int i) {
			return getToken(pascalParser.END, i);
		}
		public List<TerminalNode> SEMI() { return getTokens(pascalParser.SEMI); }
		public TerminalNode SEMI(int i) {
			return getToken(pascalParser.SEMI, i);
		}
		public List<RunningOperationContext> runningOperation() {
			return getRuleContexts(RunningOperationContext.class);
		}
		public RunningOperationContext runningOperation(int i) {
			return getRuleContext(RunningOperationContext.class,i);
		}
		public TerminalNode ELSE() { return getToken(pascalParser.ELSE, 0); }
		public IfExpressionContext ifExpression() {
			return getRuleContext(IfExpressionContext.class,0);
		}
		public IfExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ifExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterIfExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitIfExpression(this);
		}
	}

	public final IfExpressionContext ifExpression() throws RecognitionException {
		IfExpressionContext _localctx = new IfExpressionContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_ifExpression);
		((IfExpressionContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(187);
			match(IF);
			setState(188);
			match(LPAREN);
			setState(189);
			((IfExpressionContext)_localctx).condition = condition();
			setState(190);
			match(RPAREN);
			setState(191);
			match(THEN);

					_localctx.code += "if (" + ((IfExpressionContext)_localctx).condition.code + ") {";
					_localctx.code += "\n    ";
				  
			setState(202);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case BEGIN:
				{
				setState(193);
				match(BEGIN);
				setState(194);
				((IfExpressionContext)_localctx).runningScope = runningScope();
				setState(195);
				match(END);
				setState(196);
				match(SEMI);

					  		_localctx.code += ((IfExpressionContext)_localctx).runningScope.actions;
					  		_localctx.code += "}";
					  	
				}
				break;
			case DIV:
			case MOD:
			case REPEAT:
			case WHILE:
			case FOR:
			case IF:
			case READ:
			case WRITE:
			case IDENT:
				{
				setState(199);
				((IfExpressionContext)_localctx).runningOperation = runningOperation();

					  		_localctx.code += ((IfExpressionContext)_localctx).runningOperation.code;
					  		_localctx.code += "\n    ";
					  		_localctx.code += "}";
					  	  
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(220);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,16,_ctx) ) {
			case 1:
				{
				setState(204);
				match(ELSE);
				setState(217);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,15,_ctx) ) {
				case 1:
					{
					setState(205);
					match(BEGIN);
					setState(206);
					((IfExpressionContext)_localctx).runningScope = runningScope();
					setState(207);
					match(END);
					setState(208);
					match(SEMI);

						  				_localctx.code += " else {";
						  				_localctx.code += "\n    ";
						  				_localctx.code += ((IfExpressionContext)_localctx).runningScope.actions;
						  				_localctx.code += "}";
						  			
					}
					break;
				case 2:
					{
					setState(211);
					((IfExpressionContext)_localctx).runningOperation = runningOperation();

								  		_localctx.code += " else {";
						  				_localctx.code += "\n    ";
						  				_localctx.code += ((IfExpressionContext)_localctx).runningOperation.code;
						  				_localctx.code += "\n    ";
						  				_localctx.code += "}";
								  	  
					}
					break;
				case 3:
					{
					setState(214);
					((IfExpressionContext)_localctx).ifExpression = ifExpression();

						  				_localctx.code += " else ";
						  				_localctx.code += ((IfExpressionContext)_localctx).ifExpression.code;
						  			  
					}
					break;
				}
				}
				break;
			case 2:
				{
				_localctx.code += "\n";
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConditionContext extends ParserRuleContext {
		public String code;
		public RightHandExpressionContext rightHandExpression;
		public Token LT;
		public Token LE;
		public Token GE;
		public Token GT;
		public Token NOT_EQUAL;
		public Token AND;
		public Token OR;
		public Token EQUAL;
		public List<RightHandExpressionContext> rightHandExpression() {
			return getRuleContexts(RightHandExpressionContext.class);
		}
		public RightHandExpressionContext rightHandExpression(int i) {
			return getRuleContext(RightHandExpressionContext.class,i);
		}
		public TerminalNode LT() { return getToken(pascalParser.LT, 0); }
		public TerminalNode LE() { return getToken(pascalParser.LE, 0); }
		public TerminalNode GE() { return getToken(pascalParser.GE, 0); }
		public TerminalNode GT() { return getToken(pascalParser.GT, 0); }
		public TerminalNode NOT_EQUAL() { return getToken(pascalParser.NOT_EQUAL, 0); }
		public TerminalNode AND() { return getToken(pascalParser.AND, 0); }
		public TerminalNode OR() { return getToken(pascalParser.OR, 0); }
		public TerminalNode EQUAL() { return getToken(pascalParser.EQUAL, 0); }
		public ConditionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_condition; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterCondition(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitCondition(this);
		}
	}

	public final ConditionContext condition() throws RecognitionException {
		ConditionContext _localctx = new ConditionContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_condition);
		((ConditionContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(222);
			((ConditionContext)_localctx).rightHandExpression = rightHandExpression();

					_localctx.code += ((ConditionContext)_localctx).rightHandExpression.code;
				  
			setState(240);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LT:
				{
				setState(224);
				((ConditionContext)_localctx).LT = match(LT);
				_localctx.code += " " + (((ConditionContext)_localctx).LT!=null?((ConditionContext)_localctx).LT.getText():null) + " ";
				}
				break;
			case LE:
				{
				setState(226);
				((ConditionContext)_localctx).LE = match(LE);
				_localctx.code += " " + (((ConditionContext)_localctx).LE!=null?((ConditionContext)_localctx).LE.getText():null) + " ";
				}
				break;
			case GE:
				{
				setState(228);
				((ConditionContext)_localctx).GE = match(GE);
				_localctx.code += " " + (((ConditionContext)_localctx).GE!=null?((ConditionContext)_localctx).GE.getText():null) + " ";
				}
				break;
			case GT:
				{
				setState(230);
				((ConditionContext)_localctx).GT = match(GT);
				_localctx.code += " " + (((ConditionContext)_localctx).GT!=null?((ConditionContext)_localctx).GT.getText():null) + " ";
				}
				break;
			case NOT_EQUAL:
				{
				setState(232);
				((ConditionContext)_localctx).NOT_EQUAL = match(NOT_EQUAL);
				_localctx.code += " " + (((ConditionContext)_localctx).NOT_EQUAL!=null?((ConditionContext)_localctx).NOT_EQUAL.getText():null) + " ";
				}
				break;
			case AND:
				{
				setState(234);
				((ConditionContext)_localctx).AND = match(AND);
				_localctx.code += " " + (((ConditionContext)_localctx).AND!=null?((ConditionContext)_localctx).AND.getText():null) + " ";
				}
				break;
			case OR:
				{
				setState(236);
				((ConditionContext)_localctx).OR = match(OR);
				_localctx.code += " " + (((ConditionContext)_localctx).OR!=null?((ConditionContext)_localctx).OR.getText():null) + " ";
				}
				break;
			case EQUAL:
				{
				setState(238);
				((ConditionContext)_localctx).EQUAL = match(EQUAL);
				_localctx.code += " " + (((ConditionContext)_localctx).EQUAL!=null?((ConditionContext)_localctx).EQUAL.getText():null) + " ";
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(242);
			((ConditionContext)_localctx).rightHandExpression = rightHandExpression();

					_localctx.code += ((ConditionContext)_localctx).rightHandExpression.code;
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FunctionCallContext extends ParserRuleContext {
		public String code;
		public FunctionNameContext functionName;
		public VarEntriesContext varEntries;
		public FunctionNameContext functionName() {
			return getRuleContext(FunctionNameContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(pascalParser.LPAREN, 0); }
		public VarEntriesContext varEntries() {
			return getRuleContext(VarEntriesContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(pascalParser.RPAREN, 0); }
		public TerminalNode SEMI() { return getToken(pascalParser.SEMI, 0); }
		public FunctionCallContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_functionCall; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterFunctionCall(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitFunctionCall(this);
		}
	}

	public final FunctionCallContext functionCall() throws RecognitionException {
		FunctionCallContext _localctx = new FunctionCallContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_functionCall);
		((FunctionCallContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(245);
			((FunctionCallContext)_localctx).functionName = functionName();
			setState(246);
			match(LPAREN);
			setState(247);
			((FunctionCallContext)_localctx).varEntries = varEntries();
			setState(248);
			match(RPAREN);
			setState(249);
			match(SEMI);

					String [] varNames = ((FunctionCallContext)_localctx).varEntries.list.split (", ");
					if (((FunctionCallContext)_localctx).functionName.name.equals ("scanf")) {
						String template = VarManager.generateTemplate (varNames);
						_localctx.code += "scanf (\"" + template.trim () + "\", ";
						int size = varNames.length;
						for (int i = 0; i < size - 1; i ++) {
							_localctx.code += "&" + varNames [i] + ", ";
						}
						_localctx.code += "&" + varNames [size - 1] + ");";
						_localctx.code += "\n";
					} else if (((FunctionCallContext)_localctx).functionName.name.equals ("printf")) {
						String template = VarManager.generateTemplate (varNames);
						_localctx.code += "printf (\"" + template.trim () + "\\n\", ";
						int size = varNames.length;
						for (int i = 0; i < size - 1; i ++) {
							_localctx.code += varNames [i] + ", ";
						}
						_localctx.code += varNames [size - 1] + ");";
						_localctx.code += "\n";
					} else if (((FunctionCallContext)_localctx).functionName.name.equals ("div")) {
						_localctx.code += varNames [0] + " / " + varNames [1];
					} else if (((FunctionCallContext)_localctx).functionName.name.equals ("mod")) {
						_localctx.code += varNames [0] + " % " + varNames [1];
					} else {
						_localctx.code += "// ::function " + ((FunctionCallContext)_localctx).functionName.name + ":: //";
					}
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FunctionNameContext extends ParserRuleContext {
		public String name;
		public TerminalNode READ() { return getToken(pascalParser.READ, 0); }
		public TerminalNode WRITE() { return getToken(pascalParser.WRITE, 0); }
		public TerminalNode DIV() { return getToken(pascalParser.DIV, 0); }
		public TerminalNode MOD() { return getToken(pascalParser.MOD, 0); }
		public FunctionNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_functionName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterFunctionName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitFunctionName(this);
		}
	}

	public final FunctionNameContext functionName() throws RecognitionException {
		FunctionNameContext _localctx = new FunctionNameContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_functionName);
		((FunctionNameContext)_localctx).name =  "<unknown>";
		try {
			setState(260);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case READ:
				enterOuterAlt(_localctx, 1);
				{
				setState(252);
				match(READ);
				((FunctionNameContext)_localctx).name =  "scanf";
				}
				break;
			case WRITE:
				enterOuterAlt(_localctx, 2);
				{
				setState(254);
				match(WRITE);
				((FunctionNameContext)_localctx).name =  "printf";
				}
				break;
			case DIV:
				enterOuterAlt(_localctx, 3);
				{
				setState(256);
				match(DIV);
				((FunctionNameContext)_localctx).name =  "div";
				}
				break;
			case MOD:
				enterOuterAlt(_localctx, 4);
				{
				setState(258);
				match(MOD);
				((FunctionNameContext)_localctx).name =  "mod";
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ForExpressionContext extends ParserRuleContext {
		public String code;
		public boolean to = true;
		public String varName = "";;
		public IdentifierContext identifier;
		public RightHandExpressionContext rightHandExpression;
		public RunningScopeContext runningScope;
		public RunningOperationContext runningOperation;
		public TerminalNode FOR() { return getToken(pascalParser.FOR, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode ASSIGN() { return getToken(pascalParser.ASSIGN, 0); }
		public List<RightHandExpressionContext> rightHandExpression() {
			return getRuleContexts(RightHandExpressionContext.class);
		}
		public RightHandExpressionContext rightHandExpression(int i) {
			return getRuleContext(RightHandExpressionContext.class,i);
		}
		public TerminalNode DO() { return getToken(pascalParser.DO, 0); }
		public TerminalNode TO() { return getToken(pascalParser.TO, 0); }
		public TerminalNode DOWNTO() { return getToken(pascalParser.DOWNTO, 0); }
		public TerminalNode BEGIN() { return getToken(pascalParser.BEGIN, 0); }
		public RunningScopeContext runningScope() {
			return getRuleContext(RunningScopeContext.class,0);
		}
		public TerminalNode END() { return getToken(pascalParser.END, 0); }
		public TerminalNode SEMI() { return getToken(pascalParser.SEMI, 0); }
		public RunningOperationContext runningOperation() {
			return getRuleContext(RunningOperationContext.class,0);
		}
		public ForExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_forExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterForExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitForExpression(this);
		}
	}

	public final ForExpressionContext forExpression() throws RecognitionException {
		ForExpressionContext _localctx = new ForExpressionContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_forExpression);
		((ForExpressionContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(262);
			match(FOR);
			setState(263);
			((ForExpressionContext)_localctx).identifier = identifier();
			setState(264);
			match(ASSIGN);
			setState(265);
			((ForExpressionContext)_localctx).rightHandExpression = rightHandExpression();

					_localctx.code += "for (" + ((ForExpressionContext)_localctx).identifier.value + " = " 
								+ ((ForExpressionContext)_localctx).rightHandExpression.code + "; ";
				  	((ForExpressionContext)_localctx).varName =  ((ForExpressionContext)_localctx).identifier.value;
				  
			setState(270);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TO:
				{
				setState(267);
				match(TO);
				}
				break;
			case DOWNTO:
				{
				setState(268);
				match(DOWNTO);
				((ForExpressionContext)_localctx).to =  false;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(272);
			((ForExpressionContext)_localctx).rightHandExpression = rightHandExpression();
			setState(273);
			match(DO);

				  	if (_localctx.to) {
				  		_localctx.code += _localctx.varName + " <= " + ((ForExpressionContext)_localctx).rightHandExpression.code + "; ";
				  		_localctx.code += _localctx.varName + " ++) {";
				  	} else {
				  		_localctx.code += _localctx.varName + " >= " + ((ForExpressionContext)_localctx).rightHandExpression.code + "; ";
				  		_localctx.code += _localctx.varName + " --) {";
				  	}
				  	
				  	_localctx.code += "\n    ";
				  
			setState(284);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case BEGIN:
				{
				setState(275);
				match(BEGIN);
				setState(276);
				((ForExpressionContext)_localctx).runningScope = runningScope();
				setState(277);
				match(END);
				setState(278);
				match(SEMI);

					  		_localctx.code += ((ForExpressionContext)_localctx).runningScope.actions;
					  		_localctx.code += "\n    ";
					  		_localctx.code += "}";
					  	
				}
				break;
			case DIV:
			case MOD:
			case REPEAT:
			case WHILE:
			case FOR:
			case IF:
			case READ:
			case WRITE:
			case IDENT:
				{
				setState(281);
				((ForExpressionContext)_localctx).runningOperation = runningOperation();

					  		_localctx.code += ((ForExpressionContext)_localctx).runningOperation.code;
					  		_localctx.code += "\n    ";
					  		_localctx.code += "}";
					  	  
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhileExpressionContext extends ParserRuleContext {
		public String code;
		public ConditionContext condition;
		public RunningScopeContext runningScope;
		public RunningOperationContext runningOperation;
		public TerminalNode WHILE() { return getToken(pascalParser.WHILE, 0); }
		public TerminalNode LPAREN() { return getToken(pascalParser.LPAREN, 0); }
		public ConditionContext condition() {
			return getRuleContext(ConditionContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(pascalParser.RPAREN, 0); }
		public TerminalNode DO() { return getToken(pascalParser.DO, 0); }
		public TerminalNode BEGIN() { return getToken(pascalParser.BEGIN, 0); }
		public RunningScopeContext runningScope() {
			return getRuleContext(RunningScopeContext.class,0);
		}
		public TerminalNode END() { return getToken(pascalParser.END, 0); }
		public TerminalNode SEMI() { return getToken(pascalParser.SEMI, 0); }
		public RunningOperationContext runningOperation() {
			return getRuleContext(RunningOperationContext.class,0);
		}
		public WhileExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whileExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterWhileExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitWhileExpression(this);
		}
	}

	public final WhileExpressionContext whileExpression() throws RecognitionException {
		WhileExpressionContext _localctx = new WhileExpressionContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_whileExpression);
		((WhileExpressionContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(286);
			match(WHILE);
			setState(287);
			match(LPAREN);
			setState(288);
			((WhileExpressionContext)_localctx).condition = condition();
			setState(289);
			match(RPAREN);
			setState(290);
			match(DO);

					_localctx.code += "while (" + ((WhileExpressionContext)_localctx).condition.code + ") {";
				  	_localctx.code += "\n";
				  
			setState(301);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case BEGIN:
				{
				setState(292);
				match(BEGIN);
				setState(293);
				((WhileExpressionContext)_localctx).runningScope = runningScope();
				setState(294);
				match(END);
				setState(295);
				match(SEMI);

					  		_localctx.code += ((WhileExpressionContext)_localctx).runningScope.actions;
					  		_localctx.code += "\n";
					  		_localctx.code += "}";
					  	
				}
				break;
			case DIV:
			case MOD:
			case REPEAT:
			case WHILE:
			case FOR:
			case IF:
			case READ:
			case WRITE:
			case IDENT:
				{
				setState(298);
				((WhileExpressionContext)_localctx).runningOperation = runningOperation();

					  		_localctx.code += ((WhileExpressionContext)_localctx).runningOperation.code;
					  		_localctx.code += "\n";
					  		_localctx.code += "}";
					  	  
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StringExpressionContext extends ParserRuleContext {
		public String string;
		public IdentifierContext identifier;
		public Token COLON;
		public List<IdentifierContext> identifier() {
			return getRuleContexts(IdentifierContext.class);
		}
		public IdentifierContext identifier(int i) {
			return getRuleContext(IdentifierContext.class,i);
		}
		public List<TerminalNode> COLON() { return getTokens(pascalParser.COLON); }
		public TerminalNode COLON(int i) {
			return getToken(pascalParser.COLON, i);
		}
		public StringExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stringExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterStringExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitStringExpression(this);
		}
	}

	public final StringExpressionContext stringExpression() throws RecognitionException {
		StringExpressionContext _localctx = new StringExpressionContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_stringExpression);
		((StringExpressionContext)_localctx).string =  "";
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(308); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				setState(308);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case IDENT:
					{
					setState(303);
					((StringExpressionContext)_localctx).identifier = identifier();
					_localctx.string += ((StringExpressionContext)_localctx).identifier.value;
					}
					break;
				case COLON:
					{
					setState(306);
					((StringExpressionContext)_localctx).COLON = match(COLON);
					_localctx.string += (((StringExpressionContext)_localctx).COLON!=null?((StringExpressionContext)_localctx).COLON.getText():null);
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
				setState(310); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==COLON || _la==IDENT );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RepeatExpressionContext extends ParserRuleContext {
		public String code;
		public RunningScopeContext runningScope;
		public ConditionContext condition;
		public TerminalNode REPEAT() { return getToken(pascalParser.REPEAT, 0); }
		public RunningScopeContext runningScope() {
			return getRuleContext(RunningScopeContext.class,0);
		}
		public TerminalNode UNTIL() { return getToken(pascalParser.UNTIL, 0); }
		public ConditionContext condition() {
			return getRuleContext(ConditionContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(pascalParser.SEMI, 0); }
		public RepeatExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_repeatExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).enterRepeatExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof pascalListener ) ((pascalListener)listener).exitRepeatExpression(this);
		}
	}

	public final RepeatExpressionContext repeatExpression() throws RecognitionException {
		RepeatExpressionContext _localctx = new RepeatExpressionContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_repeatExpression);
		((RepeatExpressionContext)_localctx).code =  "";
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(312);
			match(REPEAT);
			setState(313);
			((RepeatExpressionContext)_localctx).runningScope = runningScope();
			setState(314);
			match(UNTIL);
			setState(315);
			((RepeatExpressionContext)_localctx).condition = condition();
			setState(316);
			match(SEMI);

					_localctx.code += "do {";
					_localctx.code += "\n";
					_localctx.code += ((RepeatExpressionContext)_localctx).runningScope.actions;
					_localctx.code += "\n";
					_localctx.code += "} while (!(" + ((RepeatExpressionContext)_localctx).condition.code + "));";
				  
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\60\u0142\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3"+
		"\3\3\3\3\3\3\4\3\4\3\4\3\5\3\5\3\5\7\5=\n\5\f\5\16\5@\13\5\3\6\3\6\3\6"+
		"\3\6\6\6F\n\6\r\6\16\6G\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3\b\3"+
		"\b\3\b\3\b\5\bX\n\b\3\t\3\t\3\t\3\t\3\t\3\t\7\t`\n\t\f\t\16\tc\13\t\3"+
		"\t\5\tf\n\t\3\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\7\13q\n\13\f\13\16"+
		"\13t\13\13\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f"+
		"\3\f\3\f\3\f\5\f\u0088\n\f\3\r\3\r\3\r\3\r\3\r\3\r\3\16\3\16\7\16\u0092"+
		"\n\16\f\16\16\16\u0095\13\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\5"+
		"\16\u009f\n\16\3\16\3\16\7\16\u00a3\n\16\f\16\16\16\u00a6\13\16\3\16\3"+
		"\16\3\16\3\16\3\16\3\16\3\16\3\16\5\16\u00b0\n\16\6\16\u00b2\n\16\r\16"+
		"\16\16\u00b3\3\16\3\16\5\16\u00b8\n\16\3\16\3\16\5\16\u00bc\n\16\3\17"+
		"\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17"+
		"\5\17\u00cd\n\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17"+
		"\3\17\3\17\5\17\u00dc\n\17\3\17\5\17\u00df\n\17\3\20\3\20\3\20\3\20\3"+
		"\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\5"+
		"\20\u00f3\n\20\3\20\3\20\3\20\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\22"+
		"\3\22\3\22\3\22\3\22\3\22\3\22\3\22\5\22\u0107\n\22\3\23\3\23\3\23\3\23"+
		"\3\23\3\23\3\23\3\23\5\23\u0111\n\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23"+
		"\3\23\3\23\3\23\3\23\3\23\5\23\u011f\n\23\3\24\3\24\3\24\3\24\3\24\3\24"+
		"\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\5\24\u0130\n\24\3\25\3\25"+
		"\3\25\3\25\3\25\6\25\u0137\n\25\r\25\16\25\u0138\3\26\3\26\3\26\3\26\3"+
		"\26\3\26\3\26\3\26\2\2\27\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&"+
		"(*\2\2\2\u0157\2,\3\2\2\2\4\61\3\2\2\2\6\66\3\2\2\2\b>\3\2\2\2\nA\3\2"+
		"\2\2\fI\3\2\2\2\16W\3\2\2\2\20e\3\2\2\2\22g\3\2\2\2\24r\3\2\2\2\26\u0087"+
		"\3\2\2\2\30\u0089\3\2\2\2\32\u00bb\3\2\2\2\34\u00bd\3\2\2\2\36\u00e0\3"+
		"\2\2\2 \u00f7\3\2\2\2\"\u0106\3\2\2\2$\u0108\3\2\2\2&\u0120\3\2\2\2(\u0136"+
		"\3\2\2\2*\u013a\3\2\2\2,-\5\4\3\2-.\5\b\5\2./\5\22\n\2/\60\b\2\1\2\60"+
		"\3\3\2\2\2\61\62\7\30\2\2\62\63\5\6\4\2\63\64\7\n\2\2\64\65\b\3\1\2\65"+
		"\5\3\2\2\2\66\67\7/\2\2\678\b\4\1\28\7\3\2\2\29:\5\n\6\2:;\b\5\1\2;=\3"+
		"\2\2\2<9\3\2\2\2=@\3\2\2\2><\3\2\2\2>?\3\2\2\2?\t\3\2\2\2@>\3\2\2\2AE"+
		"\7&\2\2BC\5\f\7\2CD\b\6\1\2DF\3\2\2\2EB\3\2\2\2FG\3\2\2\2GE\3\2\2\2GH"+
		"\3\2\2\2H\13\3\2\2\2IJ\5\20\t\2JK\7\t\2\2KL\5\16\b\2LM\7\n\2\2MN\b\7\1"+
		"\2N\r\3\2\2\2OP\7\31\2\2PX\b\b\1\2QR\7!\2\2RX\b\b\1\2ST\7\32\2\2TX\b\b"+
		"\1\2UV\7\33\2\2VX\b\b\1\2WO\3\2\2\2WQ\3\2\2\2WS\3\2\2\2WU\3\2\2\2X\17"+
		"\3\2\2\2YZ\5\6\4\2Za\b\t\1\2[\\\7\b\2\2\\]\5\6\4\2]^\b\t\1\2^`\3\2\2\2"+
		"_[\3\2\2\2`c\3\2\2\2a_\3\2\2\2ab\3\2\2\2bf\3\2\2\2ca\3\2\2\2df\3\2\2\2"+
		"eY\3\2\2\2ed\3\2\2\2f\21\3\2\2\2gh\7\37\2\2hi\5\24\13\2ij\7$\2\2jk\7\13"+
		"\2\2kl\b\n\1\2l\23\3\2\2\2mn\5\26\f\2no\b\13\1\2oq\3\2\2\2pm\3\2\2\2q"+
		"t\3\2\2\2rp\3\2\2\2rs\3\2\2\2s\25\3\2\2\2tr\3\2\2\2uv\5\30\r\2vw\b\f\1"+
		"\2w\u0088\3\2\2\2xy\5\34\17\2yz\b\f\1\2z\u0088\3\2\2\2{|\5 \21\2|}\b\f"+
		"\1\2}\u0088\3\2\2\2~\177\5$\23\2\177\u0080\b\f\1\2\u0080\u0088\3\2\2\2"+
		"\u0081\u0082\5&\24\2\u0082\u0083\b\f\1\2\u0083\u0088\3\2\2\2\u0084\u0085"+
		"\5*\26\2\u0085\u0086\b\f\1\2\u0086\u0088\3\2\2\2\u0087u\3\2\2\2\u0087"+
		"x\3\2\2\2\u0087{\3\2\2\2\u0087~\3\2\2\2\u0087\u0081\3\2\2\2\u0087\u0084"+
		"\3\2\2\2\u0088\27\3\2\2\2\u0089\u008a\5\6\4\2\u008a\u008b\7\3\2\2\u008b"+
		"\u008c\5\32\16\2\u008c\u008d\7\n\2\2\u008d\u008e\b\r\1\2\u008e\31\3\2"+
		"\2\2\u008f\u0090\7\5\2\2\u0090\u0092\b\16\1\2\u0091\u008f\3\2\2\2\u0092"+
		"\u0095\3\2\2\2\u0093\u0091\3\2\2\2\u0093\u0094\3\2\2\2\u0094\u009e\3\2"+
		"\2\2\u0095\u0093\3\2\2\2\u0096\u0097\7\60\2\2\u0097\u009f\b\16\1\2\u0098"+
		"\u0099\5\6\4\2\u0099\u009a\b\16\1\2\u009a\u009f\3\2\2\2\u009b\u009c\5"+
		" \21\2\u009c\u009d\b\16\1\2\u009d\u009f\3\2\2\2\u009e\u0096\3\2\2\2\u009e"+
		"\u0098\3\2\2\2\u009e\u009b\3\2\2\2\u009f\u00a4\3\2\2\2\u00a0\u00a1\7\6"+
		"\2\2\u00a1\u00a3\b\16\1\2\u00a2\u00a0\3\2\2\2\u00a3\u00a6\3\2\2\2\u00a4"+
		"\u00a2\3\2\2\2\u00a4\u00a5\3\2\2\2\u00a5\u00af\3\2\2\2\u00a6\u00a4\3\2"+
		"\2\2\u00a7\u00a8\7\f\2\2\u00a8\u00b0\b\16\1\2\u00a9\u00aa\7\r\2\2\u00aa"+
		"\u00b0\b\16\1\2\u00ab\u00ac\7\20\2\2\u00ac\u00b0\b\16\1\2\u00ad\u00ae"+
		"\7\21\2\2\u00ae\u00b0\b\16\1\2\u00af\u00a7\3\2\2\2\u00af\u00a9\3\2\2\2"+
		"\u00af\u00ab\3\2\2\2\u00af\u00ad\3\2\2\2\u00af\u00b0\3\2\2\2\u00b0\u00b2"+
		"\3\2\2\2\u00b1\u0093\3\2\2\2\u00b2\u00b3\3\2\2\2\u00b3\u00b1\3\2\2\2\u00b3"+
		"\u00b4\3\2\2\2\u00b4\u00bc\3\2\2\2\u00b5\u00b7\7\7\2\2\u00b6\u00b8\5("+
		"\25\2\u00b7\u00b6\3\2\2\2\u00b7\u00b8\3\2\2\2\u00b8\u00b9\3\2\2\2\u00b9"+
		"\u00ba\7\7\2\2\u00ba\u00bc\b\16\1\2\u00bb\u00b1\3\2\2\2\u00bb\u00b5\3"+
		"\2\2\2\u00bc\33\3\2\2\2\u00bd\u00be\7)\2\2\u00be\u00bf\7\5\2\2\u00bf\u00c0"+
		"\5\36\20\2\u00c0\u00c1\7\6\2\2\u00c1\u00c2\7\"\2\2\u00c2\u00cc\b\17\1"+
		"\2\u00c3\u00c4\7\37\2\2\u00c4\u00c5\5\24\13\2\u00c5\u00c6\7$\2\2\u00c6"+
		"\u00c7\7\n\2\2\u00c7\u00c8\b\17\1\2\u00c8\u00cd\3\2\2\2\u00c9\u00ca\5"+
		"\26\f\2\u00ca\u00cb\b\17\1\2\u00cb\u00cd\3\2\2\2\u00cc\u00c3\3\2\2\2\u00cc"+
		"\u00c9\3\2\2\2\u00cd\u00de\3\2\2\2\u00ce\u00db\7#\2\2\u00cf\u00d0\7\37"+
		"\2\2\u00d0\u00d1\5\24\13\2\u00d1\u00d2\7$\2\2\u00d2\u00d3\7\n\2\2\u00d3"+
		"\u00d4\b\17\1\2\u00d4\u00dc\3\2\2\2\u00d5\u00d6\5\26\f\2\u00d6\u00d7\b"+
		"\17\1\2\u00d7\u00dc\3\2\2\2\u00d8\u00d9\5\34\17\2\u00d9\u00da\b\17\1\2"+
		"\u00da\u00dc\3\2\2\2\u00db\u00cf\3\2\2\2\u00db\u00d5\3\2\2\2\u00db\u00d8"+
		"\3\2\2\2\u00dc\u00df\3\2\2\2\u00dd\u00df\b\17\1\2\u00de\u00ce\3\2\2\2"+
		"\u00de\u00dd\3\2\2\2\u00df\35\3\2\2\2\u00e0\u00e1\5\32\16\2\u00e1\u00f2"+
		"\b\20\1\2\u00e2\u00e3\7\24\2\2\u00e3\u00f3\b\20\1\2\u00e4\u00e5\7\25\2"+
		"\2\u00e5\u00f3\b\20\1\2\u00e6\u00e7\7\26\2\2\u00e7\u00f3\b\20\1\2\u00e8"+
		"\u00e9\7\27\2\2\u00e9\u00f3\b\20\1\2\u00ea\u00eb\7\23\2\2\u00eb\u00f3"+
		"\b\20\1\2\u00ec\u00ed\7%\2\2\u00ed\u00f3\b\20\1\2\u00ee\u00ef\7(\2\2\u00ef"+
		"\u00f3\b\20\1\2\u00f0\u00f1\7\22\2\2\u00f1\u00f3\b\20\1\2\u00f2\u00e2"+
		"\3\2\2\2\u00f2\u00e4\3\2\2\2\u00f2\u00e6\3\2\2\2\u00f2\u00e8\3\2\2\2\u00f2"+
		"\u00ea\3\2\2\2\u00f2\u00ec\3\2\2\2\u00f2\u00ee\3\2\2\2\u00f2\u00f0\3\2"+
		"\2\2\u00f3\u00f4\3\2\2\2\u00f4\u00f5\5\32\16\2\u00f5\u00f6\b\20\1\2\u00f6"+
		"\37\3\2\2\2\u00f7\u00f8\5\"\22\2\u00f8\u00f9\7\5\2\2\u00f9\u00fa\5\20"+
		"\t\2\u00fa\u00fb\7\6\2\2\u00fb\u00fc\7\n\2\2\u00fc\u00fd\b\21\1\2\u00fd"+
		"!\3\2\2\2\u00fe\u00ff\7,\2\2\u00ff\u0107\b\22\1\2\u0100\u0101\7-\2\2\u0101"+
		"\u0107\b\22\1\2\u0102\u0103\7\16\2\2\u0103\u0107\b\22\1\2\u0104\u0105"+
		"\7\17\2\2\u0105\u0107\b\22\1\2\u0106\u00fe\3\2\2\2\u0106\u0100\3\2\2\2"+
		"\u0106\u0102\3\2\2\2\u0106\u0104\3\2\2\2\u0107#\3\2\2\2\u0108\u0109\7"+
		"\'\2\2\u0109\u010a\5\6\4\2\u010a\u010b\7\3\2\2\u010b\u010c\5\32\16\2\u010c"+
		"\u0110\b\23\1\2\u010d\u0111\7*\2\2\u010e\u010f\7\35\2\2\u010f\u0111\b"+
		"\23\1\2\u0110\u010d\3\2\2\2\u0110\u010e\3\2\2\2\u0111\u0112\3\2\2\2\u0112"+
		"\u0113\5\32\16\2\u0113\u0114\7+\2\2\u0114\u011e\b\23\1\2\u0115\u0116\7"+
		"\37\2\2\u0116\u0117\5\24\13\2\u0117\u0118\7$\2\2\u0118\u0119\7\n\2\2\u0119"+
		"\u011a\b\23\1\2\u011a\u011f\3\2\2\2\u011b\u011c\5\26\f\2\u011c\u011d\b"+
		"\23\1\2\u011d\u011f\3\2\2\2\u011e\u0115\3\2\2\2\u011e\u011b\3\2\2\2\u011f"+
		"%\3\2\2\2\u0120\u0121\7\36\2\2\u0121\u0122\7\5\2\2\u0122\u0123\5\36\20"+
		"\2\u0123\u0124\7\6\2\2\u0124\u0125\7+\2\2\u0125\u012f\b\24\1\2\u0126\u0127"+
		"\7\37\2\2\u0127\u0128\5\24\13\2\u0128\u0129\7$\2\2\u0129\u012a\7\n\2\2"+
		"\u012a\u012b\b\24\1\2\u012b\u0130\3\2\2\2\u012c\u012d\5\26\f\2\u012d\u012e"+
		"\b\24\1\2\u012e\u0130\3\2\2\2\u012f\u0126\3\2\2\2\u012f\u012c\3\2\2\2"+
		"\u0130\'\3\2\2\2\u0131\u0132\5\6\4\2\u0132\u0133\b\25\1\2\u0133\u0137"+
		"\3\2\2\2\u0134\u0135\7\t\2\2\u0135\u0137\b\25\1\2\u0136\u0131\3\2\2\2"+
		"\u0136\u0134\3\2\2\2\u0137\u0138\3\2\2\2\u0138\u0136\3\2\2\2\u0138\u0139"+
		"\3\2\2\2\u0139)\3\2\2\2\u013a\u013b\7\34\2\2\u013b\u013c\5\24\13\2\u013c"+
		"\u013d\7 \2\2\u013d\u013e\5\36\20\2\u013e\u013f\7\n\2\2\u013f\u0140\b"+
		"\26\1\2\u0140+\3\2\2\2\32>GWaer\u0087\u0093\u009e\u00a4\u00af\u00b3\u00b7"+
		"\u00bb\u00cc\u00db\u00de\u00f2\u0106\u0110\u011e\u012f\u0136\u0138";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}