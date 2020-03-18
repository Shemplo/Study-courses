import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class A {
    
    private static final String FILE = "unimulti";
    
    public static void main (String ... args) throws IOException {
        final Path pathOut = Paths.get (FILE + ".out");
        final Path pathIn  = Paths.get (FILE + ".in");
        
        final String input = Files.readAllLines (pathIn).get (0);
        final var polynom = parse (input);
        
        final Double [][] net = new Double [23][23];
        for (int x = -10; x <= 10; x++) {
            for (int y = -10; y <= 10; y++) {
                net [y + 11][x + 11] = valueAt (polynom, x, y);
            }
        }
        
        int mins = 0, maxs = 0, plats = 0;
        for (int x = -10; x <= 10; x++) {
            for (int y = -10; y <= 10; y++) {
                final double current = net [y + 11][x + 11].doubleValue ();
                int less = 0, bigger = 0, same = 0, nulls = 0;
                
                Double value = net [y + 11 - 1][x + 11];
                if (value != null) {
                    if (value > current) { 
                        less++; 
                    } else if (value < current) { 
                        bigger++; 
                    } else {
                        same++;
                    }
                } else {
                    nulls++;
                }
                
                value = net [y + 11 + 1][x + 11];
                if (value != null) {
                    if (value > current) { 
                        less++; 
                    } else if (value < current) { 
                        bigger++; 
                    } else {
                        same++;
                    }
                } else {
                    nulls++;
                }
                
                value = net [y + 11][x + 11 - 1];
                if (value != null) {
                    if (value > current) { 
                        less++; 
                    } else if (value < current) { 
                        bigger++; 
                    } else {
                        same++;
                    }
                } else {
                    nulls++;
                }
                
                value = net [y + 11][x + 11 + 1];
                if (value != null) {
                    if (value > current) { 
                        less++; 
                    } else if (value < current) { 
                        bigger++; 
                    } else {
                        same++;
                    }
                } else {
                    nulls++;
                }
                
                if (less == 4 - nulls)   { mins++;  }
                if (bigger == 4 - nulls) { maxs++;  }
                if (same > 0) { plats++; }
            }
        }
        
        System.setOut (new PrintStream (pathOut.toFile ()));
        
        System.out.println ("Multiple local maxima: " + (maxs > 1 ? "Yes" : "No"));
        System.out.println ("Multiple local minima: " + (mins > 1 ? "Yes" : "No"));
        System.out.println ("Plateaus: " + (plats >= 1 ? "Yes" : "No"));
    }
    
    private static Double valueAt (Polynom polynom, double x, double y) {
        Double result = 0D;
        for (Monomial monomial : polynom) {
            result += monomial.apply (x, y);
        }
        
        return result;
    }
    
    private static interface Monomial extends BiFunction <Double, Double, Double> {}
    
    @SuppressWarnings ("serial")
    private static class Polynom extends ArrayList <Monomial> {}
    
    private static final Pattern MONOMIAL_PATTERN = Pattern.compile ("(\\+|-|)(\\d*)(x(\\^\\d+)?)?(y(\\^\\d+)?)?");
    
    private static Polynom parse (String input) {
        final var matcher = MONOMIAL_PATTERN.matcher (input);
        final var result = new Polynom ();
        
        while (matcher.find ()) {
            final var monomial = parseMonomial (matcher);
            if (monomial != null) { result.add (monomial); }
        }
        
        return result;
    }
    
    
    private static Monomial parseMonomial (Matcher matcher) {
        if (matcher.group ().isBlank ()) { return null; }
        //System.out.println (matcher.group ());
        
        boolean negative = matcher.group (1).equals ("-");
        //System.out.println ("Negative: " + negative);
        
        final Integer constant = Optional.ofNullable (matcher.group (2))
            . filter (value -> value.length () > 0)
            . map (Integer::parseInt)
            . orElse (1);
        //System.out.println ("Constant: " + constant);
        
        final Integer degX = Optional.ofNullable (matcher.group (3)).map (__ -> {
            return Optional.ofNullable (matcher.group (4))
                 . map (value -> Integer.parseInt (value.substring (1)))
                 . orElse (1);
        }).orElse (0);
        //System.out.println ("Degree x: " + degX);
        
        final Integer degY = Optional.ofNullable (matcher.group (5)).map (__ -> {
            return Optional.ofNullable (matcher.group (6))
                 . map (value -> Integer.parseInt (value.substring (1)))
                 . orElse (1);
        }).orElse (0);
        //System.out.println ("Degree y: " + degY);
        //System.out.println ();
        
        return (x, y) -> (negative ? -1 : 1) * constant * Math.pow (x, degX) * Math.pow (y, degY);
    }
    
}
