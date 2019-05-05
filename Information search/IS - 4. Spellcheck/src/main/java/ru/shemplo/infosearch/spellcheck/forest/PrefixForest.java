package ru.shemplo.infosearch.spellcheck.forest;

import java.util.*;
import java.util.stream.Collectors;

import ru.shemplo.infosearch.spellcheck.SearchPath;
import ru.shemplo.snowball.utils.MiscUtils;

public class PrefixForest {
    
    private static final int K = 5;
    
    //private Map <Character, PrefixTree> tries = new LinkedHashMap <> ();
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
    
    public String spellcheckWord (String word) {
        System.out.println ("Word: " + word);
        word = String.format ("^%s$", word);
        System.out.println ("Prepare: " + word);
        
        List <SearchPath> paths = new ArrayList <> ();
        
        PrefixNode stub = new PrefixNode ();
        stub.weight = 1;
        
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : word.toCharArray ()) { chars.add (c); }
        
        Comparator <SearchPath> cmp = (a, b) -> -Double.compare (a.T, b.T);
        PriorityQueue <SearchPath>  queue = new PriorityQueue <> (cmp);
        
        queue.add (new SearchPath ("", 1D));
        queue.peek ().S.push (tree);
        
        while (!queue.isEmpty ()) {
            final SearchPath path = queue.poll ();
            System.out.println ("> Prefix: " + path.F);
            if (path.F.length () < word.length () - 1) {
                for (SearchPath trans : makeTransformations (path.F, path.S, word)) {
                    System.out.println (">> Transformation: " + trans.F);
                    Character expected = word.charAt (path.F.length () + 1);
                    double prob = Optional.ofNullable (path.S.peek ().transitions.get (expected))
                                          .orElse     (stub).weight;
                    System.out.println ("Expected: " + expected + ", prob: " + prob + " / " + trans.T);
                    double fprob = path.T * (trans.T / prob) * (expected.equals (trans.F.charAt (trans.F.length () - 1)) ? 1 : 0.1);
                    SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                    //trans = trans.applyT (__ -> fprob);
                    queue.add (copy);
                }
            } else {
                if (path.S.peek ().isLeaf ()) {
                    paths.add (path);
                    
                    if (paths.size () >= K) { break; }
                } else {
                    for (@SuppressWarnings ("unused") SearchPath trans : makeTransformations (path.F, path.S, word)) {
                        //final double fprob = path.T * trans.T;
                        //trans = trans.applyT (__ -> fprob);
                        //queue.add (trans);
                    }
                }
            }
        }
        
        paths.sort (cmp);
        
        paths.forEach (System.out::println);
        return paths.get (0).F;
    }
    
    public List <SearchPath> makeTransformations (String tmp, Stack <PrefixNode> hist, String word) {
        return hist.peek ().transitions.entrySet ().stream ()
             . peek (e -> System.out.println (e.getKey () + " " + e.getValue ().weight))
             . map (e -> {
                 final Stack <PrefixNode> stack = MiscUtils.cast (hist.clone ());
                 stack.push (e.getValue ());
                 
                 final double weight = e.getValue ().weight;
                 return new SearchPath (tmp + e.getKey (), stack, weight);
             })
             . sorted  ((a, b) -> -Double.compare (a.T, b.T))
             . limit   (K)
             . peek (t -> System.out.println (t.F + " " + t.T))
             . collect (Collectors.toList ());
    }
    
}
