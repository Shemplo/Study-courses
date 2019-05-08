package ru.shemplo.infosearch.spellcheck.forest;

import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.Setter;
import ru.shemplo.infosearch.spellcheck.SearchPath;
import ru.shemplo.snowball.stuctures.Pair;

public class PrefixForest {
    
    private static final int K = 50;
    
    //private Map <Character, PrefixTree> tries = new LinkedHashMap <> ();
    @Getter @Setter
    private Map <String, Double> statistics = new HashMap <> ();
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
    
    public void normalize () {
        //tries.forEach ((__, tree) -> tree.normalize (totalCount));
        tree.normalize (totalCount);
    }
    
    public List <Pair <String, Double>> spellcheckWord (String word) {
        if (word.length () > 24) {
            return Arrays.asList (Pair.mp (word, 1D));
        }
        
        //System.out.println ("Word: " + word);
        String fword = String.format ("^%s$", word);
        //System.out.println ("Prepare: " + word);
        
        List <SearchPath> paths = new ArrayList <> ();
        
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : fword.toCharArray ()) { chars.add (c); }
        
        Comparator <SearchPath> cmp = (a, b) -> -Double.compare (a.T, b.T);
        Queue <SearchPath>  queue = new PriorityQueue <> (cmp);
        
        queue.add (new SearchPath ("", tree, 1D));
        
        while (!queue.isEmpty ()) {
            final SearchPath path = queue.poll ();
            //System.out.println ("> Prefix: " + path.F);
            if (path.F.length () < fword.length () - 1) {
                List <SearchPath> transformations = makeTransformations (path.F, path.S, fword, K);
                
                for (SearchPath trans : transformations) {
                    Character current = trans.F.charAt (trans.F.length () - 1);
                    Character expected = fword.charAt (path.F.length () + 1);
                    boolean equal = expected.equals (current);
                    String key = current + "-" + expected;
                    
                    double factor = equal ? 1 : Optional.ofNullable (statistics.get (key)).orElse (0.1D);
                    double fprob = path.T * (trans.T / path.S.weight) * factor;
                    if (!equal) {
                        final int r = path.getReplaces () + 1;
                        if (r > 1) { continue; }
                        
                        fprob *= Math.pow (1 - factor, 2.5);
                    }
                    
                    //if (fprob < 1e-10) { continue; }
                    
                    SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                    if (!equal) { copy.setReplaces (path.getReplaces () + 1); }
                    
                    queue.add (copy);
                }
            } else {
                if (path.S.isLeaf ()) {
                    paths.add (path);
                    
                    if (paths.size () >= K) { break; }
                } else if (path.F.length () - word.length () <= 2) {
                    for (SearchPath trans : makeTransformations (path.F, path.S, fword, K)) {
                        final double fprob = path.T * (trans.T / path.S.weight) * 2.5;
                        //if (fprob < 1e-20) { continue; }
                        
                        SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                        queue.add (copy);
                    }
                }
            }
        }
        
        queue.clear ();
        
        if (paths.isEmpty ()) { return Arrays.asList (Pair.mp (word, 1D)); }
        
        SearchPath best = paths.get (0);
        for (SearchPath path : paths) {
            if (best.T < path.T) {
                best = path;
            }
        }
        
        //return best.F.substring (0, best.F.length () - 1);
        return paths.stream ()
             . map (path -> Pair.mp (path.F, path.T))
             . map (pair -> pair.applyF (w -> w.substring (0, w.length () - 1)))
             . collect (Collectors.toList ());
    }
    
    public List <SearchPath> makeTransformations (String tmp, PrefixNode node, String word, int k) {
        final List <SearchPath> list = new ArrayList <> (node.transitions.size ());
        for (Entry <Character, PrefixNode> entry : node.transitions.entrySet ()) {
            final PrefixNode tnode = entry.getValue ();
            list.add (new SearchPath (tmp + entry.getKey (), tnode, tnode.weight));
        }
        
        list.sort ((a, b) -> -Double.compare (a.T, b.T));
        
        k = Math.max ((int) (node.transitions.size () * 0.75), 3);
        return list.subList (0, Math.min (k, list.size ()));
        /*
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
             */
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
