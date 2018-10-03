// Generated from Grammar.g4 by ANTLR 4.7.1

package ru.shemplo.mt.task4.antlr;

import ru.shemplo.mt.task4.CodeGenerator.Repeat;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link GrammarParser}.
 */
public interface GrammarListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link GrammarParser#grammar_file}.
	 * @param ctx the parse tree
	 */
	void enterGrammar_file(GrammarParser.Grammar_fileContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#grammar_file}.
	 * @param ctx the parse tree
	 */
	void exitGrammar_file(GrammarParser.Grammar_fileContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#grammar_rule}.
	 * @param ctx the parse tree
	 */
	void enterGrammar_rule(GrammarParser.Grammar_ruleContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#grammar_rule}.
	 * @param ctx the parse tree
	 */
	void exitGrammar_rule(GrammarParser.Grammar_ruleContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#local_variables}.
	 * @param ctx the parse tree
	 */
	void enterLocal_variables(GrammarParser.Local_variablesContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#local_variables}.
	 * @param ctx the parse tree
	 */
	void exitLocal_variables(GrammarParser.Local_variablesContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#local_variable}.
	 * @param ctx the parse tree
	 */
	void enterLocal_variable(GrammarParser.Local_variableContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#local_variable}.
	 * @param ctx the parse tree
	 */
	void exitLocal_variable(GrammarParser.Local_variableContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#grammar_rule_entry}.
	 * @param ctx the parse tree
	 */
	void enterGrammar_rule_entry(GrammarParser.Grammar_rule_entryContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#grammar_rule_entry}.
	 * @param ctx the parse tree
	 */
	void exitGrammar_rule_entry(GrammarParser.Grammar_rule_entryContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#grammar_rule_operaion}.
	 * @param ctx the parse tree
	 */
	void enterGrammar_rule_operaion(GrammarParser.Grammar_rule_operaionContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#grammar_rule_operaion}.
	 * @param ctx the parse tree
	 */
	void exitGrammar_rule_operaion(GrammarParser.Grammar_rule_operaionContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#code_expression}.
	 * @param ctx the parse tree
	 */
	void enterCode_expression(GrammarParser.Code_expressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#code_expression}.
	 * @param ctx the parse tree
	 */
	void exitCode_expression(GrammarParser.Code_expressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#code_operation}.
	 * @param ctx the parse tree
	 */
	void enterCode_operation(GrammarParser.Code_operationContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#code_operation}.
	 * @param ctx the parse tree
	 */
	void exitCode_operation(GrammarParser.Code_operationContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#operation_unit}.
	 * @param ctx the parse tree
	 */
	void enterOperation_unit(GrammarParser.Operation_unitContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#operation_unit}.
	 * @param ctx the parse tree
	 */
	void exitOperation_unit(GrammarParser.Operation_unitContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#grammar_declaration}.
	 * @param ctx the parse tree
	 */
	void enterGrammar_declaration(GrammarParser.Grammar_declarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#grammar_declaration}.
	 * @param ctx the parse tree
	 */
	void exitGrammar_declaration(GrammarParser.Grammar_declarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#ident}.
	 * @param ctx the parse tree
	 */
	void enterIdent(GrammarParser.IdentContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#ident}.
	 * @param ctx the parse tree
	 */
	void exitIdent(GrammarParser.IdentContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#expand_ident}.
	 * @param ctx the parse tree
	 */
	void enterExpand_ident(GrammarParser.Expand_identContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#expand_ident}.
	 * @param ctx the parse tree
	 */
	void exitExpand_ident(GrammarParser.Expand_identContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#regular_expression}.
	 * @param ctx the parse tree
	 */
	void enterRegular_expression(GrammarParser.Regular_expressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#regular_expression}.
	 * @param ctx the parse tree
	 */
	void exitRegular_expression(GrammarParser.Regular_expressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#character}.
	 * @param ctx the parse tree
	 */
	void enterCharacter(GrammarParser.CharacterContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#character}.
	 * @param ctx the parse tree
	 */
	void exitCharacter(GrammarParser.CharacterContext ctx);
}