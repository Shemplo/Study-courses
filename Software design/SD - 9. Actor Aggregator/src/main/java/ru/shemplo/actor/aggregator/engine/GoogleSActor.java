package ru.shemplo.actor.aggregator.engine;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import ru.shemplo.actor.aggregator.engine.units.SRequest;
import ru.shemplo.actor.aggregator.engine.units.SResponse.SResponseRow;

public class GoogleSActor extends AbsSActor {

    @Override
    protected URL makeGetRequestURL (SRequest request) {
        final String template = "https://www.googleapis.com/customsearch/v1?"
                              + "q=%s&cx=%s&num=10&key=%s";
        
        String query = null;
        try { query = URLEncoder.encode (request.getQuery (), "UTF-8"); } 
        catch (UnsupportedEncodingException uee) { /* impossible */ }
        
        final String engine = (String) configuration.get ("google.engine"),
                     key    = (String) configuration.get ("google.key");
        
        try {
            return new URL (String.format (template, query, engine, key));
        } catch (MalformedURLException murle) {
            throw new RuntimeException (murle);
        }
    }

    @Override
    protected List <SResponseRow> parseResponse (String response) {
        final List <SResponseRow> rows = new ArrayList <> ();
        final JSONObject root = new JSONObject (response);
        if (!root.has ("items")) { return rows; }
        
        JSONArray items = root.getJSONArray ("items");
        for (int i = 0; i < items.length (); i++) {
            JSONObject item = items.getJSONObject (i);
            
            final String description = item.getString ("snippet"),
                         title       = item.getString ("title"),
                         url         = item.getString ("link");
            
            URL link = null;
            try   { link = new URL (url); } 
            catch (MalformedURLException e) {}
            
            final SActorDescriptor source = SActorDescriptor.GOOGLE_ACTOR;
            rows.add (new SResponseRow (title, description, link, source));
        }
        
        return rows;
    }
    
}
