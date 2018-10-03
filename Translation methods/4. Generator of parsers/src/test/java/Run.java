import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Run {

	public static void main (String ... args) throws Exception {
		InputStream is = Files.newInputStream (Paths.get ("src/test/c/test.c"));
		Reader r = new InputStreamReader (is, StandardCharsets.UTF_8);
		
		CVariables variables = new CVariables ();
		variables.parse (r);
	}
	
}
