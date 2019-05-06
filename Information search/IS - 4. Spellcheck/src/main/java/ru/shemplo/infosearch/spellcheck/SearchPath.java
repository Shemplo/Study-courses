package ru.shemplo.infosearch.spellcheck;

import java.util.Stack;

import lombok.Getter;
import lombok.Setter;
import ru.shemplo.infosearch.spellcheck.forest.PrefixNode;
import ru.shemplo.snowball.stuctures.Trio;
import ru.shemplo.snowball.utils.MiscUtils;

public class SearchPath extends Trio <String, Stack <PrefixNode>, Double> {

    @Getter @Setter
    private int replaces = 0;
    
    public SearchPath (String F, Double T) {
        this (F, new Stack <> (), T);
    }
    
    public SearchPath (String F, Stack <PrefixNode> S, Double T) {
        super (F, S, T);
    }
    
    @Override
    public SearchPath clone () throws CloneNotSupportedException {
        Stack <PrefixNode> clone = MiscUtils.cast (S.clone ());
        return new SearchPath (F, clone, T);
    }
    
}
