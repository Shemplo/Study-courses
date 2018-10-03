import java.util.HashMap;
import java.util.Map;

public class VarManager {

	private static final Map <String, String> vars = new HashMap <> ();
	
	public static void addVar (String name, String type) {
		if (vars.containsKey (name)) {
			System.err.println ("[ERROR] Redeclaration of variable: " + name);
		}
		
		vars.put (name, type);
	}
	
	public static String generateTemplate (String... varNames) {
		if (varNames == null) { return ""; }
		
		StringBuilder sb = new StringBuilder ();
		for (String var : varNames) {
			if (!vars.containsKey (var)) {
				System.err.println ("[ERROR] Unknown variable: " + var);
				return "";
			}
			
			switch (vars.get (var)) {
				case "int":
					sb.append ("%d ");
					break;
				case "char":
					sb.append ("%c ");
					break;
				case "char *":
					sb.append ("%s ");
					break;
					
				default:
					sb.append ("");
					break;
			}
		}
		
		return sb.toString ();
	}
	
}
