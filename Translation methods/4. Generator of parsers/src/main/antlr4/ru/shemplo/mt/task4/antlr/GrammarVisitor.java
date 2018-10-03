// Generated from Grammar.g4 by ANTLR 4.7.1

package ru.shemplo.mt.task4.antlr;

import ru.shemplo.mt.task4.CodeGenerator.Repeat;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link GrammarParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface GrammarVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link GrammarParser#grammar_file}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGrammar_file(GrammarParser.Grammar_fileContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#grammar_rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGrammar_rule(GrammarParser.Grammar_ruleContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#local_variables}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLocal_variables(GrammarParser.Local_variablesContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#local_variable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLocal_variable(GrammarParser.Local_variableContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#grammar_rule_entry}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGrammar_rule_entry(GrammarParser.Grammar_rule_entryContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#grammar_rule_operaion}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGrammar_rule_operaion(GrammarParser.Grammar_rule_operaionContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#code_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCode_expression(GrammarParser.Code_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#code_operation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCode_operation(GrammarParser.Code_operationContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#operation_unit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOperation_unit(GrammarParser.Operation_unitContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#grammar_declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGrammar_declaration(GrammarParser.Grammar_declarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#ident}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdent(GrammarParser.IdentContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#expand_ident}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpand_ident(GrammarParser.Expand_identContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#regular_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRegular_expression(GrammarParser.Regular_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#character}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCharacter(GrammarParser.CharacterContext ctx);
}