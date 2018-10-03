// Generated from Grammar.g4 by ANTLR 4.7.1

package ru.shemplo.mt.task4.antlr;

import ru.shemplo.mt.task4.CodeGenerator.Repeat;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class GrammarParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, IDENT=3, NUMBER=4, WS=5, LSBRACKET=6, RSBRACKET=7, LCBRACKET=8, 
		RCBARCKET=9, ASSIGN=10, BSLASH=11, SQUOTE=12, LPAREN=13, RPAREN=14, DQUOTE=15, 
		COMMA=16, COLON=17, EQUAL=18, QMARK=19, SLASH=20, VLINE=21, DOLL=22, PLUS=23, 
		SEMI=24, STAR=25, DOT=26;
	public static final int
		RULE_grammar_file = 0, RULE_grammar_rule = 1, RULE_local_variables = 2, 
		RULE_local_variable = 3, RULE_grammar_rule_entry = 4, RULE_grammar_rule_operaion = 5, 
		RULE_code_expression = 6, RULE_code_operation = 7, RULE_operation_unit = 8, 
		RULE_grammar_declaration = 9, RULE_ident = 10, RULE_expand_ident = 11, 
		RULE_regular_expression = 12, RULE_character = 13;
	public static final String[] ruleNames = {
		"grammar_file", "grammar_rule", "local_variables", "local_variable", "grammar_rule_entry", 
		"grammar_rule_operaion", "code_expression", "code_operation", "operation_unit", 
		"grammar_declaration", "ident", "expand_ident", "regular_expression", 
		"character"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'_'", "'-'", null, null, null, "'['", "']'", "'{'", "'}'", "':='", 
		"'\\'", "'''", "'('", "')'", "'\"'", "','", "':'", "'='", "'?'", "'/'", 
		"'|'", "'$'", "'+'", "';'", "'*'", "'.'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, "IDENT", "NUMBER", "WS", "LSBRACKET", "RSBRACKET", "LCBRACKET", 
		"RCBARCKET", "ASSIGN", "BSLASH", "SQUOTE", "LPAREN", "RPAREN", "DQUOTE", 
		"COMMA", "COLON", "EQUAL", "QMARK", "SLASH", "VLINE", "DOLL", "PLUS", 
		"SEMI", "STAR", "DOT"
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
	public String getGrammarFileName() { return "Grammar.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public GrammarParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class Grammar_fileContext extends ParserRuleContext {
		public List<Grammar_ruleContext> grammar_rule() {
			return getRuleContexts(Grammar_ruleContext.class);
		}
		public Grammar_ruleContext grammar_rule(int i) {
			return getRuleContext(Grammar_ruleContext.class,i);
		}
		public List<Grammar_declarationContext> grammar_declaration() {
			return getRuleContexts(Grammar_declarationContext.class);
		}
		public Grammar_declarationContext grammar_declaration(int i) {
			return getRuleContext(Grammar_declarationContext.class,i);
		}
		public Grammar_fileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_grammar_file; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterGrammar_file(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitGrammar_file(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitGrammar_file(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Grammar_fileContext grammar_file() throws RecognitionException {
		Grammar_fileContext _localctx = new Grammar_fileContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_grammar_file);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(29); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(28);
					grammar_rule();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(31); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			setState(36);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(33);
				grammar_declaration();
				}
				}
				setState(38);
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

	public static class Grammar_ruleContext extends ParserRuleContext {
		public IdentContext ident() {
			return getRuleContext(IdentContext.class,0);
		}
		public TerminalNode LCBRACKET() { return getToken(GrammarParser.LCBRACKET, 0); }
		public TerminalNode RCBARCKET() { return getToken(GrammarParser.RCBARCKET, 0); }
		public Local_variablesContext local_variables() {
			return getRuleContext(Local_variablesContext.class,0);
		}
		public List<Grammar_rule_entryContext> grammar_rule_entry() {
			return getRuleContexts(Grammar_rule_entryContext.class);
		}
		public Grammar_rule_entryContext grammar_rule_entry(int i) {
			return getRuleContext(Grammar_rule_entryContext.class,i);
		}
		public Grammar_ruleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_grammar_rule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterGrammar_rule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitGrammar_rule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitGrammar_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Grammar_ruleContext grammar_rule() throws RecognitionException {
		Grammar_ruleContext _localctx = new Grammar_ruleContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_grammar_rule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(39);
			ident();
			setState(41);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(40);
				local_variables();
				}
			}

			setState(43);
			match(LCBRACKET);
			setState(45); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(44);
				grammar_rule_entry();
				}
				}
				setState(47); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==IDENT );
			setState(49);
			match(RCBARCKET);
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

	public static class Local_variablesContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(GrammarParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(GrammarParser.RPAREN, 0); }
		public List<Local_variableContext> local_variable() {
			return getRuleContexts(Local_variableContext.class);
		}
		public Local_variableContext local_variable(int i) {
			return getRuleContext(Local_variableContext.class,i);
		}
		public Local_variablesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_local_variables; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterLocal_variables(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitLocal_variables(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitLocal_variables(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Local_variablesContext local_variables() throws RecognitionException {
		Local_variablesContext _localctx = new Local_variablesContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_local_variables);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(51);
			match(LPAREN);
			setState(55);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(52);
				local_variable();
				}
				}
				setState(57);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(58);
			match(RPAREN);
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

	public static class Local_variableContext extends ParserRuleContext {
		public IdentContext ident() {
			return getRuleContext(IdentContext.class,0);
		}
		public Code_expressionContext code_expression() {
			return getRuleContext(Code_expressionContext.class,0);
		}
		public TerminalNode COMMA() { return getToken(GrammarParser.COMMA, 0); }
		public Local_variableContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_local_variable; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterLocal_variable(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitLocal_variable(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitLocal_variable(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Local_variableContext local_variable() throws RecognitionException {
		Local_variableContext _localctx = new Local_variableContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_local_variable);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(60);
			ident();
			setState(61);
			code_expression();
			setState(63);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(62);
				match(COMMA);
				}
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

	public static class Grammar_rule_entryContext extends ParserRuleContext {
		public Repeat repeat;
		public IdentContext ident() {
			return getRuleContext(IdentContext.class,0);
		}
		public TerminalNode PLUS() { return getToken(GrammarParser.PLUS, 0); }
		public TerminalNode QMARK() { return getToken(GrammarParser.QMARK, 0); }
		public TerminalNode STAR() { return getToken(GrammarParser.STAR, 0); }
		public Grammar_rule_operaionContext grammar_rule_operaion() {
			return getRuleContext(Grammar_rule_operaionContext.class,0);
		}
		public TerminalNode VLINE() { return getToken(GrammarParser.VLINE, 0); }
		public Grammar_rule_entryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_grammar_rule_entry; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterGrammar_rule_entry(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitGrammar_rule_entry(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitGrammar_rule_entry(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Grammar_rule_entryContext grammar_rule_entry() throws RecognitionException {
		Grammar_rule_entryContext _localctx = new Grammar_rule_entryContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_grammar_rule_entry);
		 ((Grammar_rule_entryContext)_localctx).repeat =  Repeat.SINGLE; 
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(65);
			ident();
			setState(72);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case PLUS:
				{
				setState(66);
				match(PLUS);
				 ((Grammar_rule_entryContext)_localctx).repeat =  Repeat.ONE_OR_MORE; 
				}
				break;
			case QMARK:
				{
				setState(68);
				match(QMARK);
				 ((Grammar_rule_entryContext)_localctx).repeat =  Repeat.ZERO_OR_ONE; 
				}
				break;
			case STAR:
				{
				setState(70);
				match(STAR);
				 ((Grammar_rule_entryContext)_localctx).repeat =  Repeat.ZERO_OR_MORE; 
				}
				break;
			case IDENT:
			case LCBRACKET:
			case RCBARCKET:
			case VLINE:
				break;
			default:
				break;
			}
			setState(75);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LCBRACKET) {
				{
				setState(74);
				grammar_rule_operaion();
				}
			}

			setState(78);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==VLINE) {
				{
				setState(77);
				match(VLINE);
				}
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

	public static class Grammar_rule_operaionContext extends ParserRuleContext {
		public TerminalNode LCBRACKET() { return getToken(GrammarParser.LCBRACKET, 0); }
		public TerminalNode RCBARCKET() { return getToken(GrammarParser.RCBARCKET, 0); }
		public List<Code_expressionContext> code_expression() {
			return getRuleContexts(Code_expressionContext.class);
		}
		public Code_expressionContext code_expression(int i) {
			return getRuleContext(Code_expressionContext.class,i);
		}
		public Grammar_rule_operaionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_grammar_rule_operaion; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterGrammar_rule_operaion(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitGrammar_rule_operaion(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitGrammar_rule_operaion(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Grammar_rule_operaionContext grammar_rule_operaion() throws RecognitionException {
		Grammar_rule_operaionContext _localctx = new Grammar_rule_operaionContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_grammar_rule_operaion);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(80);
			match(LCBRACKET);
			setState(84);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(81);
				code_expression();
				}
				}
				setState(86);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(87);
			match(RCBARCKET);
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

	public static class Code_expressionContext extends ParserRuleContext {
		public IdentContext ident() {
			return getRuleContext(IdentContext.class,0);
		}
		public TerminalNode EQUAL() { return getToken(GrammarParser.EQUAL, 0); }
		public Operation_unitContext operation_unit() {
			return getRuleContext(Operation_unitContext.class,0);
		}
		public Code_operationContext code_operation() {
			return getRuleContext(Code_operationContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(GrammarParser.SEMI, 0); }
		public Code_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_code_expression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterCode_expression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitCode_expression(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitCode_expression(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Code_expressionContext code_expression() throws RecognitionException {
		Code_expressionContext _localctx = new Code_expressionContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_code_expression);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(89);
			ident();
			setState(90);
			match(EQUAL);
			setState(91);
			operation_unit();
			setState(93);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==PLUS) {
				{
				setState(92);
				code_operation();
				}
			}

			setState(96);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==SEMI) {
				{
				setState(95);
				match(SEMI);
				}
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

	public static class Code_operationContext extends ParserRuleContext {
		public TerminalNode PLUS() { return getToken(GrammarParser.PLUS, 0); }
		public Operation_unitContext operation_unit() {
			return getRuleContext(Operation_unitContext.class,0);
		}
		public Code_operationContext code_operation() {
			return getRuleContext(Code_operationContext.class,0);
		}
		public Code_operationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_code_operation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterCode_operation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitCode_operation(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitCode_operation(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Code_operationContext code_operation() throws RecognitionException {
		Code_operationContext _localctx = new Code_operationContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_code_operation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(98);
			match(PLUS);
			setState(99);
			operation_unit();
			setState(101);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==PLUS) {
				{
				setState(100);
				code_operation();
				}
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

	public static class Operation_unitContext extends ParserRuleContext {
		public List<Expand_identContext> expand_ident() {
			return getRuleContexts(Expand_identContext.class);
		}
		public Expand_identContext expand_ident(int i) {
			return getRuleContext(Expand_identContext.class,i);
		}
		public List<TerminalNode> DQUOTE() { return getTokens(GrammarParser.DQUOTE); }
		public TerminalNode DQUOTE(int i) {
			return getToken(GrammarParser.DQUOTE, i);
		}
		public List<TerminalNode> SLASH() { return getTokens(GrammarParser.SLASH); }
		public TerminalNode SLASH(int i) {
			return getToken(GrammarParser.SLASH, i);
		}
		public List<TerminalNode> BSLASH() { return getTokens(GrammarParser.BSLASH); }
		public TerminalNode BSLASH(int i) {
			return getToken(GrammarParser.BSLASH, i);
		}
		public List<TerminalNode> STAR() { return getTokens(GrammarParser.STAR); }
		public TerminalNode STAR(int i) {
			return getToken(GrammarParser.STAR, i);
		}
		public List<TerminalNode> LPAREN() { return getTokens(GrammarParser.LPAREN); }
		public TerminalNode LPAREN(int i) {
			return getToken(GrammarParser.LPAREN, i);
		}
		public List<TerminalNode> RPAREN() { return getTokens(GrammarParser.RPAREN); }
		public TerminalNode RPAREN(int i) {
			return getToken(GrammarParser.RPAREN, i);
		}
		public List<TerminalNode> LCBRACKET() { return getTokens(GrammarParser.LCBRACKET); }
		public TerminalNode LCBRACKET(int i) {
			return getToken(GrammarParser.LCBRACKET, i);
		}
		public List<TerminalNode> RCBARCKET() { return getTokens(GrammarParser.RCBARCKET); }
		public TerminalNode RCBARCKET(int i) {
			return getToken(GrammarParser.RCBARCKET, i);
		}
		public List<TerminalNode> SEMI() { return getTokens(GrammarParser.SEMI); }
		public TerminalNode SEMI(int i) {
			return getToken(GrammarParser.SEMI, i);
		}
		public List<TerminalNode> NUMBER() { return getTokens(GrammarParser.NUMBER); }
		public TerminalNode NUMBER(int i) {
			return getToken(GrammarParser.NUMBER, i);
		}
		public List<TerminalNode> EQUAL() { return getTokens(GrammarParser.EQUAL); }
		public TerminalNode EQUAL(int i) {
			return getToken(GrammarParser.EQUAL, i);
		}
		public List<TerminalNode> PLUS() { return getTokens(GrammarParser.PLUS); }
		public TerminalNode PLUS(int i) {
			return getToken(GrammarParser.PLUS, i);
		}
		public Operation_unitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_operation_unit; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterOperation_unit(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitOperation_unit(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitOperation_unit(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Operation_unitContext operation_unit() throws RecognitionException {
		Operation_unitContext _localctx = new Operation_unitContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_operation_unit);
		int _la;
		try {
			setState(123);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENT:
			case DOLL:
				enterOuterAlt(_localctx, 1);
				{
				setState(103);
				expand_ident();
				}
				break;
			case DQUOTE:
				enterOuterAlt(_localctx, 2);
				{
				setState(104);
				match(DQUOTE);
				setState(119);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << IDENT) | (1L << NUMBER) | (1L << LCBRACKET) | (1L << RCBARCKET) | (1L << BSLASH) | (1L << LPAREN) | (1L << RPAREN) | (1L << EQUAL) | (1L << SLASH) | (1L << DOLL) | (1L << PLUS) | (1L << SEMI) | (1L << STAR))) != 0)) {
					{
					setState(117);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case IDENT:
					case DOLL:
						{
						setState(105);
						expand_ident();
						}
						break;
					case SLASH:
						{
						setState(106);
						match(SLASH);
						}
						break;
					case BSLASH:
						{
						setState(107);
						match(BSLASH);
						}
						break;
					case STAR:
						{
						setState(108);
						match(STAR);
						}
						break;
					case LPAREN:
						{
						setState(109);
						match(LPAREN);
						}
						break;
					case RPAREN:
						{
						setState(110);
						match(RPAREN);
						}
						break;
					case LCBRACKET:
						{
						setState(111);
						match(LCBRACKET);
						}
						break;
					case RCBARCKET:
						{
						setState(112);
						match(RCBARCKET);
						}
						break;
					case SEMI:
						{
						setState(113);
						match(SEMI);
						}
						break;
					case NUMBER:
						{
						setState(114);
						match(NUMBER);
						}
						break;
					case EQUAL:
						{
						setState(115);
						match(EQUAL);
						}
						break;
					case PLUS:
						{
						setState(116);
						match(PLUS);
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					}
					setState(121);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(122);
				match(DQUOTE);
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

	public static class Grammar_declarationContext extends ParserRuleContext {
		public IdentContext ident() {
			return getRuleContext(IdentContext.class,0);
		}
		public TerminalNode COLON() { return getToken(GrammarParser.COLON, 0); }
		public TerminalNode SEMI() { return getToken(GrammarParser.SEMI, 0); }
		public Expand_identContext expand_ident() {
			return getRuleContext(Expand_identContext.class,0);
		}
		public Regular_expressionContext regular_expression() {
			return getRuleContext(Regular_expressionContext.class,0);
		}
		public CharacterContext character() {
			return getRuleContext(CharacterContext.class,0);
		}
		public Grammar_declarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_grammar_declaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterGrammar_declaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitGrammar_declaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitGrammar_declaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Grammar_declarationContext grammar_declaration() throws RecognitionException {
		Grammar_declarationContext _localctx = new Grammar_declarationContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_grammar_declaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(125);
			ident();
			setState(126);
			match(COLON);
			setState(130);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENT:
			case DOLL:
				{
				setState(127);
				expand_ident();
				}
				break;
			case SLASH:
				{
				setState(128);
				regular_expression();
				}
				break;
			case SQUOTE:
				{
				setState(129);
				character();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(132);
			match(SEMI);
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

	public static class IdentContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(GrammarParser.IDENT, 0); }
		public IdentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ident; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterIdent(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitIdent(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitIdent(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdentContext ident() throws RecognitionException {
		IdentContext _localctx = new IdentContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_ident);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(134);
			match(IDENT);
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

	public static class Expand_identContext extends ParserRuleContext {
		public List<TerminalNode> IDENT() { return getTokens(GrammarParser.IDENT); }
		public TerminalNode IDENT(int i) {
			return getToken(GrammarParser.IDENT, i);
		}
		public TerminalNode DOLL() { return getToken(GrammarParser.DOLL, 0); }
		public List<TerminalNode> DOT() { return getTokens(GrammarParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(GrammarParser.DOT, i);
		}
		public Expand_identContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expand_ident; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterExpand_ident(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitExpand_ident(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitExpand_ident(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Expand_identContext expand_ident() throws RecognitionException {
		Expand_identContext _localctx = new Expand_identContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_expand_ident);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(137);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==DOLL) {
				{
				setState(136);
				match(DOLL);
				}
			}

			setState(139);
			match(IDENT);
			setState(143);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(140);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << IDENT) | (1L << DOT))) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					}
					} 
				}
				setState(145);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
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

	public static class Regular_expressionContext extends ParserRuleContext {
		public List<TerminalNode> SLASH() { return getTokens(GrammarParser.SLASH); }
		public TerminalNode SLASH(int i) {
			return getToken(GrammarParser.SLASH, i);
		}
		public List<TerminalNode> IDENT() { return getTokens(GrammarParser.IDENT); }
		public TerminalNode IDENT(int i) {
			return getToken(GrammarParser.IDENT, i);
		}
		public List<TerminalNode> LPAREN() { return getTokens(GrammarParser.LPAREN); }
		public TerminalNode LPAREN(int i) {
			return getToken(GrammarParser.LPAREN, i);
		}
		public List<TerminalNode> RPAREN() { return getTokens(GrammarParser.RPAREN); }
		public TerminalNode RPAREN(int i) {
			return getToken(GrammarParser.RPAREN, i);
		}
		public List<TerminalNode> LSBRACKET() { return getTokens(GrammarParser.LSBRACKET); }
		public TerminalNode LSBRACKET(int i) {
			return getToken(GrammarParser.LSBRACKET, i);
		}
		public List<TerminalNode> RSBRACKET() { return getTokens(GrammarParser.RSBRACKET); }
		public TerminalNode RSBRACKET(int i) {
			return getToken(GrammarParser.RSBRACKET, i);
		}
		public List<TerminalNode> DOT() { return getTokens(GrammarParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(GrammarParser.DOT, i);
		}
		public List<TerminalNode> STAR() { return getTokens(GrammarParser.STAR); }
		public TerminalNode STAR(int i) {
			return getToken(GrammarParser.STAR, i);
		}
		public List<TerminalNode> PLUS() { return getTokens(GrammarParser.PLUS); }
		public TerminalNode PLUS(int i) {
			return getToken(GrammarParser.PLUS, i);
		}
		public List<TerminalNode> QMARK() { return getTokens(GrammarParser.QMARK); }
		public TerminalNode QMARK(int i) {
			return getToken(GrammarParser.QMARK, i);
		}
		public List<TerminalNode> BSLASH() { return getTokens(GrammarParser.BSLASH); }
		public TerminalNode BSLASH(int i) {
			return getToken(GrammarParser.BSLASH, i);
		}
		public List<TerminalNode> COMMA() { return getTokens(GrammarParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(GrammarParser.COMMA, i);
		}
		public List<TerminalNode> COLON() { return getTokens(GrammarParser.COLON); }
		public TerminalNode COLON(int i) {
			return getToken(GrammarParser.COLON, i);
		}
		public List<TerminalNode> EQUAL() { return getTokens(GrammarParser.EQUAL); }
		public TerminalNode EQUAL(int i) {
			return getToken(GrammarParser.EQUAL, i);
		}
		public List<TerminalNode> NUMBER() { return getTokens(GrammarParser.NUMBER); }
		public TerminalNode NUMBER(int i) {
			return getToken(GrammarParser.NUMBER, i);
		}
		public Regular_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_regular_expression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterRegular_expression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitRegular_expression(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitRegular_expression(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Regular_expressionContext regular_expression() throws RecognitionException {
		Regular_expressionContext _localctx = new Regular_expressionContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_regular_expression);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(146);
			match(SLASH);
			setState(148); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(147);
				_la = _input.LA(1);
				if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__1) | (1L << IDENT) | (1L << NUMBER) | (1L << LSBRACKET) | (1L << RSBRACKET) | (1L << BSLASH) | (1L << LPAREN) | (1L << RPAREN) | (1L << COMMA) | (1L << COLON) | (1L << EQUAL) | (1L << QMARK) | (1L << PLUS) | (1L << STAR) | (1L << DOT))) != 0)) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				}
				}
				setState(150); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__1) | (1L << IDENT) | (1L << NUMBER) | (1L << LSBRACKET) | (1L << RSBRACKET) | (1L << BSLASH) | (1L << LPAREN) | (1L << RPAREN) | (1L << COMMA) | (1L << COLON) | (1L << EQUAL) | (1L << QMARK) | (1L << PLUS) | (1L << STAR) | (1L << DOT))) != 0) );
			setState(152);
			match(SLASH);
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

	public static class CharacterContext extends ParserRuleContext {
		public CharacterContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_character; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).enterCharacter(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrammarListener ) ((GrammarListener)listener).exitCharacter(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrammarVisitor ) return ((GrammarVisitor<? extends T>)visitor).visitCharacter(this);
			else return visitor.visitChildren(this);
		}
	}

	public final CharacterContext character() throws RecognitionException {
		CharacterContext _localctx = new CharacterContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_character);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(154);
			match(SQUOTE);
			setState(155);
			matchWildcard();
			setState(156);
			match(SQUOTE);
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\34\u00a1\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\3\2\6\2 \n\2\r\2\16\2!\3\2\7"+
		"\2%\n\2\f\2\16\2(\13\2\3\3\3\3\5\3,\n\3\3\3\3\3\6\3\60\n\3\r\3\16\3\61"+
		"\3\3\3\3\3\4\3\4\7\48\n\4\f\4\16\4;\13\4\3\4\3\4\3\5\3\5\3\5\5\5B\n\5"+
		"\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6K\n\6\3\6\5\6N\n\6\3\6\5\6Q\n\6\3\7\3"+
		"\7\7\7U\n\7\f\7\16\7X\13\7\3\7\3\7\3\b\3\b\3\b\3\b\5\b`\n\b\3\b\5\bc\n"+
		"\b\3\t\3\t\3\t\5\th\n\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3"+
		"\n\3\n\3\n\7\nx\n\n\f\n\16\n{\13\n\3\n\5\n~\n\n\3\13\3\13\3\13\3\13\3"+
		"\13\5\13\u0085\n\13\3\13\3\13\3\f\3\f\3\r\5\r\u008c\n\r\3\r\3\r\7\r\u0090"+
		"\n\r\f\r\16\r\u0093\13\r\3\16\3\16\6\16\u0097\n\16\r\16\16\16\u0098\3"+
		"\16\3\16\3\17\3\17\3\17\3\17\3\17\2\2\20\2\4\6\b\n\f\16\20\22\24\26\30"+
		"\32\34\2\4\5\2\3\3\5\5\34\34\t\2\3\6\b\t\r\r\17\20\22\25\31\31\33\34\2"+
		"\u00b3\2\37\3\2\2\2\4)\3\2\2\2\6\65\3\2\2\2\b>\3\2\2\2\nC\3\2\2\2\fR\3"+
		"\2\2\2\16[\3\2\2\2\20d\3\2\2\2\22}\3\2\2\2\24\177\3\2\2\2\26\u0088\3\2"+
		"\2\2\30\u008b\3\2\2\2\32\u0094\3\2\2\2\34\u009c\3\2\2\2\36 \5\4\3\2\37"+
		"\36\3\2\2\2 !\3\2\2\2!\37\3\2\2\2!\"\3\2\2\2\"&\3\2\2\2#%\5\24\13\2$#"+
		"\3\2\2\2%(\3\2\2\2&$\3\2\2\2&\'\3\2\2\2\'\3\3\2\2\2(&\3\2\2\2)+\5\26\f"+
		"\2*,\5\6\4\2+*\3\2\2\2+,\3\2\2\2,-\3\2\2\2-/\7\n\2\2.\60\5\n\6\2/.\3\2"+
		"\2\2\60\61\3\2\2\2\61/\3\2\2\2\61\62\3\2\2\2\62\63\3\2\2\2\63\64\7\13"+
		"\2\2\64\5\3\2\2\2\659\7\17\2\2\668\5\b\5\2\67\66\3\2\2\28;\3\2\2\29\67"+
		"\3\2\2\29:\3\2\2\2:<\3\2\2\2;9\3\2\2\2<=\7\20\2\2=\7\3\2\2\2>?\5\26\f"+
		"\2?A\5\16\b\2@B\7\22\2\2A@\3\2\2\2AB\3\2\2\2B\t\3\2\2\2CJ\5\26\f\2DE\7"+
		"\31\2\2EK\b\6\1\2FG\7\25\2\2GK\b\6\1\2HI\7\33\2\2IK\b\6\1\2JD\3\2\2\2"+
		"JF\3\2\2\2JH\3\2\2\2JK\3\2\2\2KM\3\2\2\2LN\5\f\7\2ML\3\2\2\2MN\3\2\2\2"+
		"NP\3\2\2\2OQ\7\27\2\2PO\3\2\2\2PQ\3\2\2\2Q\13\3\2\2\2RV\7\n\2\2SU\5\16"+
		"\b\2TS\3\2\2\2UX\3\2\2\2VT\3\2\2\2VW\3\2\2\2WY\3\2\2\2XV\3\2\2\2YZ\7\13"+
		"\2\2Z\r\3\2\2\2[\\\5\26\f\2\\]\7\24\2\2]_\5\22\n\2^`\5\20\t\2_^\3\2\2"+
		"\2_`\3\2\2\2`b\3\2\2\2ac\7\32\2\2ba\3\2\2\2bc\3\2\2\2c\17\3\2\2\2de\7"+
		"\31\2\2eg\5\22\n\2fh\5\20\t\2gf\3\2\2\2gh\3\2\2\2h\21\3\2\2\2i~\5\30\r"+
		"\2jy\7\21\2\2kx\5\30\r\2lx\7\26\2\2mx\7\r\2\2nx\7\33\2\2ox\7\17\2\2px"+
		"\7\20\2\2qx\7\n\2\2rx\7\13\2\2sx\7\32\2\2tx\7\6\2\2ux\7\24\2\2vx\7\31"+
		"\2\2wk\3\2\2\2wl\3\2\2\2wm\3\2\2\2wn\3\2\2\2wo\3\2\2\2wp\3\2\2\2wq\3\2"+
		"\2\2wr\3\2\2\2ws\3\2\2\2wt\3\2\2\2wu\3\2\2\2wv\3\2\2\2x{\3\2\2\2yw\3\2"+
		"\2\2yz\3\2\2\2z|\3\2\2\2{y\3\2\2\2|~\7\21\2\2}i\3\2\2\2}j\3\2\2\2~\23"+
		"\3\2\2\2\177\u0080\5\26\f\2\u0080\u0084\7\23\2\2\u0081\u0085\5\30\r\2"+
		"\u0082\u0085\5\32\16\2\u0083\u0085\5\34\17\2\u0084\u0081\3\2\2\2\u0084"+
		"\u0082\3\2\2\2\u0084\u0083\3\2\2\2\u0085\u0086\3\2\2\2\u0086\u0087\7\32"+
		"\2\2\u0087\25\3\2\2\2\u0088\u0089\7\5\2\2\u0089\27\3\2\2\2\u008a\u008c"+
		"\7\30\2\2\u008b\u008a\3\2\2\2\u008b\u008c\3\2\2\2\u008c\u008d\3\2\2\2"+
		"\u008d\u0091\7\5\2\2\u008e\u0090\t\2\2\2\u008f\u008e\3\2\2\2\u0090\u0093"+
		"\3\2\2\2\u0091\u008f\3\2\2\2\u0091\u0092\3\2\2\2\u0092\31\3\2\2\2\u0093"+
		"\u0091\3\2\2\2\u0094\u0096\7\26\2\2\u0095\u0097\t\3\2\2\u0096\u0095\3"+
		"\2\2\2\u0097\u0098\3\2\2\2\u0098\u0096\3\2\2\2\u0098\u0099\3\2\2\2\u0099"+
		"\u009a\3\2\2\2\u009a\u009b\7\26\2\2\u009b\33\3\2\2\2\u009c\u009d\7\16"+
		"\2\2\u009d\u009e\13\2\2\2\u009e\u009f\7\16\2\2\u009f\35\3\2\2\2\26!&+"+
		"\619AJMPV_bgwy}\u0084\u008b\u0091\u0098";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}