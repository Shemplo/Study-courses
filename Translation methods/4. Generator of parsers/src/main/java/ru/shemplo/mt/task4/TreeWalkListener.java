package ru.shemplo.mt.task4;

import java.util.LinkedList;
import java.util.List;

import ru.shemplo.dsau.stuctures.Trio;
import ru.shemplo.mt.task4.CodeGenerator.Repeat;
import ru.shemplo.mt.task4.antlr.GrammarBaseListener;
import ru.shemplo.mt.task4.antlr.GrammarParser.Code_expressionContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Code_operationContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Grammar_declarationContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Grammar_ruleContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Grammar_rule_entryContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Grammar_rule_operaionContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Local_variableContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Local_variablesContext;
import ru.shemplo.mt.task4.antlr.GrammarParser.Operation_unitContext;
import ru.shemplo.mt.task4.dmy.NonTerminal;
import ru.shemplo.mt.task4.dmy.Terminal;

public class TreeWalkListener extends GrammarBaseListener {

	private final GrammarCollector COLLECTOR;
	
	public TreeWalkListener (final GrammarCollector collector) {
		this.COLLECTOR = collector;
	}
	
	@Override
	public void enterGrammar_rule (Grammar_ruleContext ctx) {
		String name = ctx.ident ().IDENT ().getText ();
		System.out.println ("Enter to / " + name);
		NonTerminal rule = new NonTerminal (name);
		COLLECTOR.addNonTerminal (rule);
		
		if (ctx.local_variables () != null) {
			Local_variablesContext localVars = ctx.local_variables ();
			System.out.println ("/ " + name + " has local variables:");
			for (Local_variableContext localVar : localVars.local_variable ()) {
				Code_expressionContext codeExpr = localVar.code_expression ();
				String type    = localVar.ident ().IDENT ().getText (),
					   varName = codeExpr.ident ().IDENT ().getText (),
					   value   = codeExpr.operation_unit ().getText ();
				
				Code_operationContext operation = codeExpr.code_operation ();
				if (operation != null) { value += operation.getText (); }
				value = CodeGenerator.replaceSnippets (value);
				
				System.out.println ("~ " + type + " " + varName + " = " + value);
				rule.addLocalVariable (type, varName, value);
			}
		}
		
		List <Trio <String, Repeat, String>> branch = new LinkedList <> ();
		for (Grammar_rule_entryContext ruleEntry : ctx.grammar_rule_entry ()) {
			String dependRule = ruleEntry.ident ().IDENT ().getText ();
			Repeat repeat = ruleEntry.repeat;
			String action = "";
			
			Grammar_rule_operaionContext operaion = ruleEntry.grammar_rule_operaion ();
			if (operaion != null) {
				List <Code_expressionContext> codes = operaion.code_expression ();
				if (codes != null) {
					StringBuilder sb = new StringBuilder ();
					for (Code_expressionContext code : codes) {
						String leftName = code.ident ().getText ();
						sb.append ("((#) context).").append (leftName).append ("=");
						
						Operation_unitContext unit = code.operation_unit ();
						String unitStr = unit.getText ();
						
						if (unitStr.charAt (0) == '$') {
							unitStr = unitStr.replaceAll ("\\$(\\w|[.])+[.](\\w+)", "$2");
						} else if (unitStr.charAt (0) != '"') {
							sb.append ("((#) context).");
						}
						
						sb.append (unitStr);
						
						Code_operationContext coc = code.code_operation ();
						while (coc != null) {
							sb.append (coc.PLUS ().getText ());
							unit = coc.operation_unit ();
							unitStr = unit.getText ();
							
							if (unitStr.charAt (0) == '$') {
								unitStr = unitStr.replaceAll ("\\$(\\w|[.])+[.](\\w+)", "$2");
							} else if (unitStr.charAt (0) != '"') {
								sb.append ("((#) context).");
							}
							
							sb.append (unitStr);
							coc = coc.code_operation ();
						}
					}
					
					action = sb.toString ().replace ("$space", "\" \"")
							   .replace ("$s", "\" \"").replace ("$dq", "\"\\\"\"");
				}
			}
			branch.add (Trio.mt (dependRule, repeat, action));
			
			if (ruleEntry.VLINE () != null) {
				System.out.println ("/ " + name + " has alternative branch: ");
				for (Trio <String, Repeat, String> entry : branch) {
					System.out.println ("> " + entry);
				}
				
				rule.addAlternamtive (branch);
				branch = new LinkedList <> ();
			}
		}
		
		if (branch.size () > 0) {
			System.out.println ("/ " + name + " has alternative branch: ");
			for (Trio <String, Repeat, String> entry : branch) {
				System.out.println ("> " + entry);
			}
			
			rule.addAlternamtive (branch);
		}
		
		System.out.println ();
	}
	
	@Override
	public void enterGrammar_declaration (Grammar_declarationContext ctx) {
		String name = ctx.ident ().IDENT ().getText ();
		//System.out.println ("Enter to | " + name);
		
		boolean isRegexp = false;
		String pattern = "";
		if (ctx.expand_ident () != null) {
			pattern = ctx.expand_ident ().getText ();
			isRegexp = false;
		} else if (ctx.regular_expression () != null) {
			pattern = ctx.regular_expression ().getText ().replace ("/", "");
			isRegexp = true;
		} else if (ctx.character () != null) {
			pattern = ctx.character ().getText ().replace ("'", "");
			isRegexp = false;
		}
		
		pattern = CodeGenerator.replaceSnippets (pattern);
		
		Terminal rule = new Terminal (name, pattern, isRegexp);
		COLLECTOR.addTerminal (rule);
	}
	
}
