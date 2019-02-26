package ru.shemplo.actor.aggregator.engine;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ru.shemplo.actor.aggregator.engine.units.SRequest;
import ru.shemplo.actor.aggregator.engine.units.SResponse.SResponseRow;

public class YahooSActor extends AbsSActor {

    private static final String REGEXP_TEMPLATE = "<div class=\"(\\w|\\s)+result\">.*?"
                                                + "<a href=\"(.*?)\" class=\"\">(.*?)</a>.*?"
                                                + "<div class=\"s\">.*?"
                                                + "<p class=\"abstract ellipsis\">(.*?)</p>.*?"
                                                + "</div>.*?</div>";
    private static final Pattern REGEXP_PATTERN = Pattern.compile (REGEXP_TEMPLATE, Pattern.MULTILINE);
    
    @Override
    protected URL makeGetRequestURL (SRequest request) {
        final String template = "https://search.yahoo.com/search?p=%s&fr=yfp-t&fp=1&toggle=1&cop=mss&ei=UTF-8";
        try   { return new URL (String.format (template, URLEncoder.encode (request.getQuery (), "UTF-8"))); } 
        catch (MalformedURLException | UnsupportedEncodingException e) { throw new RuntimeException (e); }
    }

    @Override
    protected List <SResponseRow> parseResponse (String response) {
        final List <SResponseRow> rows = new ArrayList <> ();
        Matcher matcher = REGEXP_PATTERN.matcher (response);
        
        matcher.find (); // Skipping first line (it's navigation bar)
        
        while (matcher.find ()) {
            try {
                final String title = matcher.group (3), headline = matcher.group (4),
                        url = matcher.group (2);
                final URL link = new URL (url);
                
                final SActorDescriptor source = SActorDescriptor.YAHOO_ACTOR;
                rows.add (new SResponseRow (title, headline, link, source));
            } catch (MalformedURLException murle) {
                throw new RuntimeException (murle);
            }
        }
        
        return rows;
    }
    
}
