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
    
    @Getter @Setter
    private Map <String, Double> statistics = new HashMap <> ();
    private final PrefixNode tree = new PrefixNode ();
    
    private int totalCount = 0;
    
    /**
     * Adds word to the prefix trie.
     * 
     * @param word that should be added
     * 
     * @param count number of word appeared in search queries
     * 
     */
    public void addWord (String word, int count) {
        word = String.format ("^%s$", word);
        totalCount += count;
        
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : word.toCharArray ()) { chars.add (c); }
        
        tree.processSequence (chars, count);
    }
    
    public void normalize () {
        tree.normalize (totalCount);
    }
    
    /**
     * Gets the word and makes attempts to correct it using
     * prior probability in Prefix Forest.
     * 
     * @param word to spellcheck and correct
     * 
     * @return sorted list of candidates for correction
     * 
     */
    public List <Pair <String, Double>> spellcheckWord (String word) {
        /*
         * If word is too long then say that we can't do anything
         */
        if (word.length () > 24) {
            return Arrays.asList (Pair.mp (word, 1D));
        }
        
        // Wrapping word with operational symbols ^ and $
        String fword = String.format ("^%s$", word);
        
        List <SearchPath> paths = new ArrayList <> ();
        
        final Queue <Character> chars = new LinkedList <> ();
        for (char c : fword.toCharArray ()) { chars.add (c); }
        
        Comparator <SearchPath> cmp = (a, b) -> -Double.compare (a.T, b.T);
        Queue <SearchPath>  queue = new PriorityQueue <> (cmp);
        
        queue.add (new SearchPath ("", tree, 1D));
        
        while (!queue.isEmpty ()) {
            final SearchPath path = queue.poll ();
            
            // If we didn't get enough length of word in tree
            if (path.F.length () < fword.length () - 1) {
                // Fetching some transitions with the biggest prior probability
                List <SearchPath> transformations = makeTransformations (path.F, path.S, fword);
                
                for (SearchPath trans : transformations) {
                    Character current = trans.F.charAt (trans.F.length () - 1);
                    Character expected = fword.charAt (path.F.length () + 1);
                    boolean equal = expected.equals (current);
                    String key = current + "-" + expected;
                    
                    // Estimate probability that current symbol is not correct and tax should be taken
                    double factor = equal ? 1 : Optional.ofNullable (statistics.get (key)).orElse (0.1D);
                    double fprob = path.T * (trans.T / path.S.weight) * factor;
                    if (!equal) {
                        final int r = path.getReplaces () + 1;
                        if (r > 1) { continue; }
                        
                        // Making tax for deviation from the word
                        fprob *= Math.pow (1 - factor, 2.5);
                        
                        /*
                         * Tax doesn't excepts candidate from the list at all
                         * but decrease it position in such list.
                         * 
                         * This is done from the suggestion that in words
                         * there are 2 mistakes (replaces) on average. So
                         * if deviation is more than 2 symbols than tax will
                         * be huge and such candidate won't be on top.
                         */
                    }
                    
                    //if (fprob < 1e-10) { continue; }
                    
                    SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                    if (!equal) { copy.setReplaces (path.getReplaces () + 1); }
                    
                    queue.add (copy);
                }
                
            // If we got enough length of word in tree
            } else {
                // In terminal node we just remember the candidate and it's weight
                if (path.S.isLeaf ()) {
                    paths.add (path);
                    
                    if (paths.size () >= K) { break; }
                    
                // Also make attempts to add some symbols to the end to expand word
                } else if (path.F.length () - word.length () <= 2) {
                    for (SearchPath trans : makeTransformations (path.F, path.S, fword)) {
                        final double fprob = path.T * (trans.T / path.S.weight) * 2.5;
                        //if (fprob < 1e-20) { continue; }
                        
                        SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                        queue.add (copy);
                    }
                }
            }
        }
        
        queue.clear ();
        
        // If absolutely nothing in tree than just return given word
        if (paths.isEmpty ()) { return Arrays.asList (Pair.mp (word, 1D)); }
        
        // Converting result to appropriate form
        return paths.stream ()
             . map (path -> Pair.mp (path.F, path.T))
             . map (pair -> pair.applyF (w -> w.substring (0, w.length () - 1)))
             . collect (Collectors.toList ());
    }
    
    /**
     * For given state (node) in tree generates candidate transitions by choosing 
     * the most poplar variants. For each node will be returned 75% of possible
     * transitions (or 3 if 75% is less).
     * 
     * @param tmp current substring in tree search state
     * @param node in tree search state
     * @param word that should be spellchecked
     * 
     * @return list of possible transitions
     * 
     */
    public List <SearchPath> makeTransformations (String tmp, PrefixNode node, String word) {
        final List <SearchPath> list = new ArrayList <> (node.transitions.size ());
        for (Entry <Character, PrefixNode> entry : node.transitions.entrySet ()) {
            final PrefixNode tnode = entry.getValue ();
            list.add (new SearchPath (tmp + entry.getKey (), tnode, tnode.weight));
        }
        
        list.sort ((a, b) -> -Double.compare (a.T, b.T));
        
        int k = Math.max ((int) (node.transitions.size () * 0.75), 3);
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
