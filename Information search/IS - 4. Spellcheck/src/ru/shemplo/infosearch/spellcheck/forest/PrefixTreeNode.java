package ru.shemplo.infosearch.spellcheck.forest;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Queue;

public class PrefixTreeNode {
    
    protected final Map <Character, PrefixTreeNode> 
        transitions = new LinkedHashMap <> ();
    protected double norm = 1, weight = 0;
    protected Character value;
    
    public void processSequence (Queue <Character> sequence, int count) {
        this.value = sequence.poll ();
        this.weight = count;
        
        if (sequence.size () > 1) {
            final Character ancor = sequence.peek ();
            
            transitions.putIfAbsent (ancor,    new PrefixTreeNode ());
            transitions.get (ancor).processSequence (sequence, count);
        }
    }
    
    public void normalize (double norm) {
        if (this.norm == 1) {
            this.norm = norm;
            weight /= norm;
        }
        
        transitions.forEach ((__, tree) -> tree.normalize (norm));
    }
    
    protected boolean isLeaf () {
        return transitions.isEmpty ();
    }
    
}
