package ru.shemplo.infosearch.spellcheck.forest;

import java.util.Queue;

public class PrefixTree extends PrefixTreeNode {
    
    public void addSequence (Queue <Character> sequence, int count) {
        processSequence (sequence, count);
    }
    
}
