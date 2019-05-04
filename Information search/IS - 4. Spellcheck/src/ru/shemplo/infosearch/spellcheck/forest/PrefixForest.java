package ru.shemplo.infosearch.spellcheck.forest;

import java.util.*;
import java.util.stream.Collectors;

import ru.shemplo.infosearch.spellcheck.Trio;

public class PrefixForest {
    
    private static final int K = 5;
    
    //private Map <Character, PrefixTree> tries = new LinkedHashMap <> ();
    private final PrefixTreeNode tree = new PrefixTreeNode ();
    
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
        word = String.format ("^%s$", word);
        
        List <Trio <String, PrefixTreeNode, Double>> 
            paths = new ArrayList <> ();
        
        PrefixTreeNode stub = new PrefixTreeNode ();
        stub.weight = 1;
        
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : word.toCharArray ()) { chars.add (c); }
        
        Comparator <Trio <String, PrefixTreeNode, Double>> 
            cmp = (a, b) -> -Double.compare (a.T, b.T);
        PriorityQueue <Trio <String, PrefixTreeNode, Double>> 
            queue = new PriorityQueue <> (cmp);
        queue.add (Trio.mt ("", tree, 1D));
        
        while (queue.isEmpty ()) {
            Trio <String, PrefixTreeNode, Double> path = queue.poll ();
            if (path.F.length () < word.length ()) {
                for (Trio <String, PrefixTreeNode, Double> trans : 
                        makeTransformations (path.F, tree, word)) {
                    Character expected = word.charAt (word.length ());
                    double prob = Optional.ofNullable (path.S.transitions.get (expected))
                                          .orElse     (stub).weight;
                    double fprob = path.T * (trans.T / prob) * 1; // XXX: 1 == P(pi, hist | theta) ????
                    trans = trans.applyT (__ -> fprob);
                    queue.add (trans);
                }
            } else {
                if (path.S.isLeaf ()) {
                    paths.add (path);
                    
                    if (paths.size () >= K) { break; }
                } else {
                    for (Trio <String, PrefixTreeNode, Double> trans : 
                            makeTransformations (path.F, tree, word)) {
                        Character expected = word.charAt (word.length ());
                        double prob = Optional.ofNullable (path.S.transitions.get (expected))
                                              .orElse     (stub).weight;
                        double fprob = path.T * (trans.T / prob);
                        trans = trans.applyT (__ -> fprob);
                        queue.add (trans);
                    }
                }
            }
        }
        
        paths.sort (cmp);
        return paths.get (0).F;
    }
    
    public List <Trio <String, PrefixTreeNode, Double>> makeTransformations (String tmp,
            PrefixTreeNode node, String word) {
        return node.transitions.entrySet ().stream ()
             . map (e -> Trio.mt (
                     e.getKey (), 
                     e.getValue (), 
                     e.getValue ().weight
                   )
             )
             . map     (t -> t.applyF (v -> tmp + v))
             . sorted  ((a, b) -> -Double.compare (a.T, b.T))
             . limit   (K)
             . collect (Collectors.toList ());
    }
    
}
