package ru.shemplo.infosearch.spellcheck.forest;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Queue;

import lombok.ToString;
import ru.shemplo.snowball.stuctures.Pair;

@ToString (exclude = {"transitions"})
public class PrefixNode {
    
    protected final Map <Character, PrefixNode> 
        transitions = new LinkedHashMap <> ();
    protected double norm = 1, weight = 0;
    protected Character value;
    
    public void processSequence (Queue <Character> sequence, int count) {
        weight = Math.max (weight, count);
        this.value = sequence.poll ();
        
        if (sequence.size () > 0) {
            final Character ancor = sequence.peek ();
            
            transitions.putIfAbsent (ancor,        new PrefixNode ());
            transitions.get (ancor).processSequence (sequence, count);
        }
    }
    
    public void normalize (double norm) {
        if (this.norm == 1) {
            this.norm = norm;
            weight /= norm;
        }
        
        transitions.forEach ((__, tree) -> tree.normalize (norm));
        
        /*
        Map <Character, Pair <Double, PrefixNode>> 
            deletions = new HashMap <> ();
        transitions.forEach ((__, node) -> {
            node.transitions.forEach ((ch, dest) -> {
                if (transitions.containsKey (ch)) { return; }
                deletions.compute (ch, (___, pair) -> {
                    return pair == null
                         ? Pair.mp (dest.weight, dest)
                         : pair.F < dest.weight
                         ? Pair.mp (dest.weight, dest)
                         : pair;
                });
            });
        });
        
        deletions.forEach ((ch, pair) -> {
            transitions.put (ch, pair.S);
        });
        */
    }
    
    protected boolean isLeaf () {
        return transitions.isEmpty ();
    }
    
}
