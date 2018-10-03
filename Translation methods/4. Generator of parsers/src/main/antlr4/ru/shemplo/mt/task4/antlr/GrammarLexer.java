// Generated from Grammar.g4 by ANTLR 4.7.1

package ru.shemplo.mt.task4.antlr;

import ru.shemplo.mt.task4.CodeGenerator.Repeat;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class GrammarLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, IDENT=3, NUMBER=4, WS=5, LSBRACKET=6, RSBRACKET=7, LCBRACKET=8, 
		RCBARCKET=9, ASSIGN=10, BSLASH=11, SQUOTE=12, LPAREN=13, RPAREN=14, DQUOTE=15, 
		COMMA=16, COLON=17, EQUAL=18, QMARK=19, SLASH=20, VLINE=21, DOLL=22, PLUS=23, 
		SEMI=24, STAR=25, DOT=26;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "IDENT", "NUMBER", "WS", "LSBRACKET", "RSBRACKET", "LCBRACKET", 
		"RCBARCKET", "ASSIGN", "BSLASH", "SQUOTE", "LPAREN", "RPAREN", "DQUOTE", 
		"COMMA", "COLON", "EQUAL", "QMARK", "SLASH", "VLINE", "DOLL", "PLUS", 
		"SEMI", "STAR", "DOT"
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


	public GrammarLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Grammar.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\34v\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\3\2\3\2\3\3\3\3\3\4\3\4\7\4>\n\4\f\4\16\4A\13\4\3"+
		"\5\6\5D\n\5\r\5\16\5E\3\6\3\6\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n"+
		"\3\13\3\13\3\13\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3\21"+
		"\3\22\3\22\3\23\3\23\3\24\3\24\3\25\3\25\3\26\3\26\3\27\3\27\3\30\3\30"+
		"\3\31\3\31\3\32\3\32\3\33\3\33\2\2\34\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21"+
		"\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30"+
		"/\31\61\32\63\33\65\34\3\2\5\4\2C\\c|\6\2\62;C\\aac|\5\2\13\f\17\17\""+
		"\"\2w\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r"+
		"\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2"+
		"\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2"+
		"#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3"+
		"\2\2\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\3\67\3\2\2\2\59\3\2\2\2"+
		"\7;\3\2\2\2\tC\3\2\2\2\13G\3\2\2\2\rK\3\2\2\2\17M\3\2\2\2\21O\3\2\2\2"+
		"\23Q\3\2\2\2\25S\3\2\2\2\27V\3\2\2\2\31X\3\2\2\2\33Z\3\2\2\2\35\\\3\2"+
		"\2\2\37^\3\2\2\2!`\3\2\2\2#b\3\2\2\2%d\3\2\2\2\'f\3\2\2\2)h\3\2\2\2+j"+
		"\3\2\2\2-l\3\2\2\2/n\3\2\2\2\61p\3\2\2\2\63r\3\2\2\2\65t\3\2\2\2\678\7"+
		"a\2\28\4\3\2\2\29:\7/\2\2:\6\3\2\2\2;?\t\2\2\2<>\t\3\2\2=<\3\2\2\2>A\3"+
		"\2\2\2?=\3\2\2\2?@\3\2\2\2@\b\3\2\2\2A?\3\2\2\2BD\4\62;\2CB\3\2\2\2DE"+
		"\3\2\2\2EC\3\2\2\2EF\3\2\2\2F\n\3\2\2\2GH\t\4\2\2HI\3\2\2\2IJ\b\6\2\2"+
		"J\f\3\2\2\2KL\7]\2\2L\16\3\2\2\2MN\7_\2\2N\20\3\2\2\2OP\7}\2\2P\22\3\2"+
		"\2\2QR\7\177\2\2R\24\3\2\2\2ST\7<\2\2TU\7?\2\2U\26\3\2\2\2VW\7^\2\2W\30"+
		"\3\2\2\2XY\7)\2\2Y\32\3\2\2\2Z[\7*\2\2[\34\3\2\2\2\\]\7+\2\2]\36\3\2\2"+
		"\2^_\7$\2\2_ \3\2\2\2`a\7.\2\2a\"\3\2\2\2bc\7<\2\2c$\3\2\2\2de\7?\2\2"+
		"e&\3\2\2\2fg\7A\2\2g(\3\2\2\2hi\7\61\2\2i*\3\2\2\2jk\7~\2\2k,\3\2\2\2"+
		"lm\7&\2\2m.\3\2\2\2no\7-\2\2o\60\3\2\2\2pq\7=\2\2q\62\3\2\2\2rs\7,\2\2"+
		"s\64\3\2\2\2tu\7\60\2\2u\66\3\2\2\2\5\2?E\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}