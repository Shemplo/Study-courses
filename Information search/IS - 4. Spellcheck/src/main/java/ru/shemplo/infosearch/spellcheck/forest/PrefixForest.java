package ru.shemplo.infosearch.spellcheck.forest;

import java.util.*;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.Setter;
import ru.shemplo.infosearch.spellcheck.SearchPath;

public class PrefixForest {
    
    private static final int K = 35;
    
    //private Map <Character, PrefixTree> tries = new LinkedHashMap <> ();
    @Getter @Setter
    private Map <String, Double> statistics = new HashMap <> ();
    private final Set <String> correct = new HashSet <> ();
    private final PrefixNode tree = new PrefixNode ();
    
    private int totalCount = 0;
    
    public void addWord (String word, int count) {
        word = String.format ("^%s$", word);
        totalCount += count;
        
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : word.toCharArray ()) { chars.add (c); }
        
        //tries.putIfAbsent (chars.peek (), new PrefixTree ());
        //tries.get (chars.peek ()).addSequence (chars, count);
        tree.processSequence (chars, count);
    }
    
    public void addCorrectWord (String word) {
        correct.add (word);
    }
    
    public void normalize () {
        //tries.forEach ((__, tree) -> tree.normalize (totalCount));
        tree.normalize (totalCount);
    }
    
    public String spellcheckWord (String word) {
        if (correct.contains (word)) {
            System.out.println ("Correct!");
            return word;
        }
        
        //System.out.println ("Word: " + word);
        word = String.format ("^%s$", word);
        //System.out.println ("Prepare: " + word);
        
        List <SearchPath> paths = new ArrayList <> ();
        
        PrefixNode stub = new PrefixNode ();
        stub.weight = 1;
        
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : word.toCharArray ()) { chars.add (c); }
        
        Comparator <SearchPath> cmp = (a, b) -> -Double.compare (a.T, b.T);
        PriorityQueue <SearchPath>  queue = new PriorityQueue <> (cmp);
        
        queue.add (new SearchPath ("", tree, 1D));
        
        while (!queue.isEmpty ()) {
            final SearchPath path = queue.poll ();
            //System.out.println ("> Prefix: " + path.F);
            if (path.F.length () < word.length () - 1) {
                List <SearchPath> transformations = makeTransformations (path.F, path.S, word);
                
                for (SearchPath trans : transformations) {
                    Character current = trans.F.charAt (trans.F.length () - 1);
                    Character expected = word.charAt (path.F.length () + 1);
                    boolean equal = expected.equals (current);
                    String key = current + "-" + expected;
                    
                    double factor = equal ? 1 : Optional.ofNullable (statistics.get (key)).orElse (0.01D);
                    double fprob = path.T * (trans.T / path.S.weight) * factor;
                    if (!equal) {
                        final int r = path.getReplaces () + 1;
                        fprob *= Math.pow (Math.E, -r * 2.5);
                    }
                    
                    SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                    if (!equal) {
                        copy.setReplaces (path.getReplaces () + 1);
                    }
                    
                    queue.add (copy);
                }
            } else {
                if (path.S.isLeaf ()) {
                    paths.add (path);
                    
                    if (paths.size () >= K) { break; }
                }
                
                for (SearchPath trans : makeTransformations (path.F, path.S, word)) {
                    final double fprob = path.T * (trans.T / path.S.weight) * 2;
                    SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                    queue.add (copy);
                }
            }
        }
        
        paths.sort (cmp);
        
        //paths.forEach (System.out::println);
        int length = paths.get (0).F.length ();
        return paths.get (0).F.substring (0, length - 1);
    }
    
    public List <SearchPath> makeTransformations (String tmp, PrefixNode node, String word) {
        return node.transitions.entrySet ().stream ()
             //. peek (e -> System.out.println (e.getKey () + " " + e.getValue ().weight))
             . map (e -> {
                 final double weight = e.getValue ().weight;
                 return new SearchPath (tmp + e.getKey (), e.getValue (), weight);
             })
             . sorted  ((a, b) -> -Double.compare (a.T, b.T))
             . limit   (K)
             //. peek (t -> System.out.println (t.F + " " + t.T))
             . collect (Collectors.toList ());
    }
    
    public void findWord (String word) {
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : word.toCharArray ()) { chars.add (c); }
        
        PrefixNode node = tree;
        int step = 0;
        
        while (!chars.isEmpty ()) {
            Character c = chars.poll ();
            if (!node.transitions.containsKey (c)) {
                System.out.println ("Word not find: " + c + " / " + step);
            }
            
            node = node.transitions.get (c);
            step += 1;
        }
        
        System.out.println ("Word find " + node.weight);
        node.transitions.forEach ((key, value) -> {
            System.out.println ("Transition: " + key + " / " + value.isLeaf () + " / " + value.weight);
        });
    }
    
}
