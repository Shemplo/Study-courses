// Generated from pascal.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link pascalParser}.
 */
public interface pascalListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link pascalParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(pascalParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(pascalParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#programHeading}.
	 * @param ctx the parse tree
	 */
	void enterProgramHeading(pascalParser.ProgramHeadingContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#programHeading}.
	 * @param ctx the parse tree
	 */
	void exitProgramHeading(pascalParser.ProgramHeadingContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#identifier}.
	 * @param ctx the parse tree
	 */
	void enterIdentifier(pascalParser.IdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#identifier}.
	 * @param ctx the parse tree
	 */
	void exitIdentifier(pascalParser.IdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#programVarDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterProgramVarDeclaration(pascalParser.ProgramVarDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#programVarDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitProgramVarDeclaration(pascalParser.ProgramVarDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#varDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterVarDeclaration(pascalParser.VarDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#varDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitVarDeclaration(pascalParser.VarDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#varEntryLine}.
	 * @param ctx the parse tree
	 */
	void enterVarEntryLine(pascalParser.VarEntryLineContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#varEntryLine}.
	 * @param ctx the parse tree
	 */
	void exitVarEntryLine(pascalParser.VarEntryLineContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#varType}.
	 * @param ctx the parse tree
	 */
	void enterVarType(pascalParser.VarTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#varType}.
	 * @param ctx the parse tree
	 */
	void exitVarType(pascalParser.VarTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#varEntries}.
	 * @param ctx the parse tree
	 */
	void enterVarEntries(pascalParser.VarEntriesContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#varEntries}.
	 * @param ctx the parse tree
	 */
	void exitVarEntries(pascalParser.VarEntriesContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#mainRunningScope}.
	 * @param ctx the parse tree
	 */
	void enterMainRunningScope(pascalParser.MainRunningScopeContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#mainRunningScope}.
	 * @param ctx the parse tree
	 */
	void exitMainRunningScope(pascalParser.MainRunningScopeContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#runningScope}.
	 * @param ctx the parse tree
	 */
	void enterRunningScope(pascalParser.RunningScopeContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#runningScope}.
	 * @param ctx the parse tree
	 */
	void exitRunningScope(pascalParser.RunningScopeContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#runningOperation}.
	 * @param ctx the parse tree
	 */
	void enterRunningOperation(pascalParser.RunningOperationContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#runningOperation}.
	 * @param ctx the parse tree
	 */
	void exitRunningOperation(pascalParser.RunningOperationContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#assignment}.
	 * @param ctx the parse tree
	 */
	void enterAssignment(pascalParser.AssignmentContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#assignment}.
	 * @param ctx the parse tree
	 */
	void exitAssignment(pascalParser.AssignmentContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#rightHandExpression}.
	 * @param ctx the parse tree
	 */
	void enterRightHandExpression(pascalParser.RightHandExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#rightHandExpression}.
	 * @param ctx the parse tree
	 */
	void exitRightHandExpression(pascalParser.RightHandExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#ifExpression}.
	 * @param ctx the parse tree
	 */
	void enterIfExpression(pascalParser.IfExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#ifExpression}.
	 * @param ctx the parse tree
	 */
	void exitIfExpression(pascalParser.IfExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#condition}.
	 * @param ctx the parse tree
	 */
	void enterCondition(pascalParser.ConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#condition}.
	 * @param ctx the parse tree
	 */
	void exitCondition(pascalParser.ConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#functionCall}.
	 * @param ctx the parse tree
	 */
	void enterFunctionCall(pascalParser.FunctionCallContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#functionCall}.
	 * @param ctx the parse tree
	 */
	void exitFunctionCall(pascalParser.FunctionCallContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#functionName}.
	 * @param ctx the parse tree
	 */
	void enterFunctionName(pascalParser.FunctionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#functionName}.
	 * @param ctx the parse tree
	 */
	void exitFunctionName(pascalParser.FunctionNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#forExpression}.
	 * @param ctx the parse tree
	 */
	void enterForExpression(pascalParser.ForExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#forExpression}.
	 * @param ctx the parse tree
	 */
	void exitForExpression(pascalParser.ForExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#whileExpression}.
	 * @param ctx the parse tree
	 */
	void enterWhileExpression(pascalParser.WhileExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#whileExpression}.
	 * @param ctx the parse tree
	 */
	void exitWhileExpression(pascalParser.WhileExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#stringExpression}.
	 * @param ctx the parse tree
	 */
	void enterStringExpression(pascalParser.StringExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#stringExpression}.
	 * @param ctx the parse tree
	 */
	void exitStringExpression(pascalParser.StringExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalParser#repeatExpression}.
	 * @param ctx the parse tree
	 */
	void enterRepeatExpression(pascalParser.RepeatExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalParser#repeatExpression}.
	 * @param ctx the parse tree
	 */
	void exitRepeatExpression(pascalParser.RepeatExpressionContext ctx);
}