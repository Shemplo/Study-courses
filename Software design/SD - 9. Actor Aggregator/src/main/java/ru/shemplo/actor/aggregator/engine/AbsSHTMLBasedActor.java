package ru.shemplo.actor.aggregator.engine;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ru.shemplo.actor.aggregator.engine.units.SResponse.SResponseRow;

public abstract class AbsSHTMLBasedActor extends AbsSActor {
    
    protected Pattern pattern = null;
    
    protected abstract Pattern getFetchingPattern ();
    
    protected int [] groups = null;
    
    protected abstract int [] getFetchingGroups ();
    
    protected abstract SActorDescriptor getActor ();
    
    @Override
    protected List <SResponseRow> parseResponse (String response) {
        final List <SResponseRow> rows = new ArrayList <> ();
        
        if (pattern == null) { pattern = getFetchingPattern (); }
        Matcher matcher = pattern.matcher (response);
        
        matcher.find (); // Skipping first line (it's navigation bar)
        
        while (matcher.find ()) {
            try {
                if (groups == null) { groups = getFetchingGroups (); }
                
                final String title = matcher.group (groups [0]), 
                        headline = matcher.group (groups [1]),
                        url    = matcher.group (groups [2]);
                final URL link = new URL (url);
                
                rows.add (new SResponseRow (title, headline, link, getActor ()));
            } catch (MalformedURLException murle) {
                throw new RuntimeException (murle);
            }
        }
        
        return rows;
    }
    
}
