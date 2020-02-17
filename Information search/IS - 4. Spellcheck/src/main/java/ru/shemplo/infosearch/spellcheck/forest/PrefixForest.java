package ru.shemplo.infosearch.spellcheck.forest;

import static java.util.Optional.*;

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
    private Map <String, Double> stats = new HashMap <> ();
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
        
        queue.add (new SearchPath ("^", tree, 1D, 0));
        
        //System.out.println (makeTransformations ("", tree, fword));
        
        while (!queue.isEmpty ()) {
            final SearchPath path = queue.poll ();
            
            // If we didn't get enough length of word in tree
            if (path.F.length () < fword.length ()) {
                // Fetching some transitions with the biggest prior probability
                List <SearchPath> transformations = makeTransformations (path.F, path.S, fword);
                
                for (SearchPath trans : transformations) {
                    int offset = path.getOffset () + trans.getOffset ();
                    
                    Character current  = trans.F.charAt (trans.F.length () - 1); // last character
                    Character expected = fword.charAt (path.F.length () - offset);
                    boolean equal = expected.equals (current);
                    /*
                    if (word.equals ("КОРОЛЫ")) {
                        System.out.println (trans);
                        System.out.println (current + " / " + expected + " / " 
                                       + equal + " / " + path.getReplaces ());
                        try {
                            Thread.sleep (200);
                        } catch (InterruptedException e) {}
                    }
                    */
                    //String key = "" + current;// + "-" + expected;
                    
                    // Estimate probability that current symbol is not correct and tax should be taken
                    double factor  = equal ? 0 : ofNullable (stats.get (expected + "-" + current)).orElse (0.001D);
                    double factorE = equal ? 0 : ofNullable (stats.get ("" + expected)).orElse (0.001D);
                    double factorC = equal ? 0 : ofNullable (stats.get ("" + current)).orElse (0.001D);
                    double fprob = path.T * (trans.T / path.S.weight);
                    if (equal) { fprob *= Math.pow (1 - factorC, 4); } 
                    else {
                        final int r = path.getReplaces () + 1;
                        if (r > 1) { continue; }
                        
                        // Making tax for deviation from the word
                        fprob *= Math.pow (factor * (1 - factorE), 5);
                        
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
                    
                    SearchPath copy = new SearchPath (trans.F, trans.S, fprob, offset);
                    if (!equal) { copy.setReplaces (path.getReplaces () + 1); }
                    else        { copy.setReplaces (path.getReplaces ()); }
                    
                    //System.out.println (copy + " " + copy.getReplaces ());
                    queue.add (copy);
                }
                
            // If we got enough length of word in tree
            } else {
                // In terminal node we just remember the candidate and it's weight
                if (path.S.isLeaf ()) {
                    if (word.equals ("КОРОЛЫ")) {
                        System.out.println (path);
                    }
                    paths.add (path);
                    
                    if (paths.size () >= K) { break; }
                    
                // Also make attempts to add some symbols to the end to expand word
                } /*else if (path.F.length () - word.length () <= 2) {
                    for (SearchPath trans : makeTransformations (path.F, path.S, fword)) {
                        final double fprob = path.T * (trans.T / path.S.weight) * 2.5;
                        //if (fprob < 1e-20) { continue; }
                        
                        SearchPath copy = new SearchPath (trans.F, trans.S, fprob);
                        queue.add (copy);
                    }
                }*/
            }
        }
        
        queue.clear ();
        
        // If absolutely nothing in tree than just return given word
        if (paths.isEmpty ()) { return Arrays.asList (Pair.mp (word, 1D)); }
        
        // Converting result to appropriate form
        return paths.stream ()
             . map (path -> Pair.mp (path.F, path.T))
             . map (pair -> pair.applyF (w -> w.substring (1, w.length () - 1)))
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
            list.add (new SearchPath (tmp + entry.getKey (), tnode, tnode.weight, 0));
            
            // insertion
            list.add (new SearchPath (tmp + entry.getKey (), node, tnode.weight, 1));
        }
        
        if (tmp.length () > 1) {
            final String string = tmp.substring (0, tmp.length () - 1);
            list.add (new SearchPath (string, node, node.weight, -1));            
        }
        
        list.sort ((a, b) -> -Double.compare (a.T, b.T));
        
        int k = Math.max ((int) (node.transitions.size () * 0.75), 3);
        return list.subList (0, Math.min (k, list.size ()));
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
