package ru.shemplo.infosearch.spellcheck;

import lombok.Getter;
import lombok.Setter;
import ru.shemplo.infosearch.spellcheck.forest.PrefixNode;
import ru.shemplo.snowball.stuctures.Trio;

public class SearchPath extends Trio <String, PrefixNode, Double> {

    @Getter @Setter
    private int replaces = 0;
    
    @Getter @Setter
    private int offset = 0;
    
    public SearchPath (String F, PrefixNode S, Double T, int offset) {
        super (F, S, T);
        
        this.offset = offset;
    }
    
    /*
    @Override
    public SearchPath clone () throws CloneNotSupportedException {
        Stack <PrefixNode> clone = MiscUtils.cast (S.clone ());
        return new SearchPath (F, clone, T);
    }
    */
    
}
