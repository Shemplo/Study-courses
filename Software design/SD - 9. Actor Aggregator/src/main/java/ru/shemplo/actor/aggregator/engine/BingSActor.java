package ru.shemplo.actor.aggregator.engine;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;

import java.util.regex.Pattern;

import lombok.Getter;
import ru.shemplo.actor.aggregator.engine.units.SRequest;

public class BingSActor extends AbsSHTMLBasedActor {

    private static final String REGEXP_TEMPLATE = "<li class=\"b_algo\">.*?"
                                                + "<a href=\"(.*?)\" h=\"[\\w\\d\\.,=]*\">(.*?)</a>"
                                                + ".*?<p>(.*?)</p>.*?</li>";
    private static final Pattern REGEXP_PATTERN = Pattern.compile (REGEXP_TEMPLATE, Pattern.MULTILINE);
    
    @Getter private final SActorDescriptor actor = SActorDescriptor.BING_ACTOR;
    @Getter private final Pattern fetchingPattern = REGEXP_PATTERN;
    @Getter private final int [] fetchingGroups = {2, 3, 1};
    
    @Override
    protected URL makeGetRequestURL (SRequest request) {
        final String template = "https://www.bing.com/search?q=%s&qs=n&form=QBLH&sp=-1&pq=&sc=0-0"
                              + "&sk=&cvid=310F3D715F904CBA8ED8409B6792CEC9";
        try   { return new URL (String.format (template, URLEncoder.encode (request.getQuery (), "UTF-8"))); } 
        catch (MalformedURLException | UnsupportedEncodingException e) { throw new RuntimeException (e); }
    }
    
}
