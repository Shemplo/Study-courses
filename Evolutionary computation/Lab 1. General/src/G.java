import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class G {
    
    private static final int K = 23, R = 13;
    
    private static int n, fitness;
    
    public static void main (String ... args) throws IOException {
        final var br = new BufferedReader (new InputStreamReader (System.in));
        
        final var nNfitness = br.readLine ().split (" ");
        fitness = Integer.parseInt (nNfitness [1]);
        n = Integer.parseInt (nNfitness [0]);
        
        if (n == fitness) {
            System.exit (0);
        }
        
        int ft = fitness;
        for (int i = 0; i < ft; i++) {
            final int a = Integer.parseInt (br.readLine ());
            fitness = processBlock (i, fitness, a);
        }
    }
    
    public static int processBlock (int i, int fitness, int a) {
        
        
        return fitness;
    }
    
}
