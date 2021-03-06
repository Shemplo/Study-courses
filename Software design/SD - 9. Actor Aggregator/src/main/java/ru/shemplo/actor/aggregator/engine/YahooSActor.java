package ru.shemplo.actor.aggregator.engine;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;

import java.util.regex.Pattern;

import lombok.Getter;
import ru.shemplo.actor.aggregator.engine.units.SRequest;

public class YahooSActor extends AbsSHTMLBasedActor {

    private static final String REGEXP_TEMPLATE = "<div class=\"(\\w|\\s)+result\">.*?"
                                                + "<a href=\"(.*?)\" class=\"\">(.*?)</a>.*?"
                                                + "<div class=\"s\">.*?"
                                                + "<p class=\"abstract ellipsis\">(.*?)</p>.*?"
                                                + "</div>.*?</div>";
    private static final Pattern REGEXP_PATTERN = Pattern.compile (REGEXP_TEMPLATE, Pattern.MULTILINE);
    
    @Getter private final SActorDescriptor actor = SActorDescriptor.YAHOO_ACTOR;
    @Getter private final Pattern fetchingPattern = REGEXP_PATTERN;
    @Getter private final int [] fetchingGroups = {3, 4, 2};
    
    @Override
    protected URL makeGetRequestURL (SRequest request) {
        final String template = "https://search.yahoo.com/search?p=%s&fr=yfp-t&fp=1&toggle=1&cop=mss&ei=UTF-8";
        try   { return new URL (String.format (template, URLEncoder.encode (request.getQuery (), "UTF-8"))); } 
        catch (MalformedURLException | UnsupportedEncodingException e) { throw new RuntimeException (e); }
    }
    
}
