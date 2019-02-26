package ru.shemplo.actor.aggregator.engine;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import java.util.ArrayList;
import java.util.List;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import ru.shemplo.actor.aggregator.engine.units.SRequest;
import ru.shemplo.actor.aggregator.engine.units.SResponse.SResponseRow;
import ru.shemplo.actor.aggregator.engine.xml.StaxStream;

public class YandexSActor extends AbsSActor {
    
    @Override
    protected URL makeGetRequestURL (SRequest request) throws RuntimeException {
        final String template = "https://yandex.com/search/xml?l10n=en&user=%s&key=%s&query=%s";
        
        String query = null;
        try { query = URLEncoder.encode (request.getQuery (), "UTF-8"); } 
        catch (UnsupportedEncodingException uee) { /* impossible */ }
        
        final String user = (String) configuration.get ("yandex.user"),
                     key  = (String) configuration.get ("yandex.key");
        
        try {
            return new URL (String.format (template, user, key, query));
        } catch (MalformedURLException murle) {
            throw new RuntimeException (murle);
        }
    }
    
    @Override
    protected List <SResponseRow> parseResponse (String response) {
        response = response.replace ("<hlword>", "").replace ("</hlword>", "");
        final byte [] bytes = response.getBytes (StandardCharsets.UTF_8);
        InputStream is = new ByteArrayInputStream (bytes);
        
        List <SResponseRow> rows = new ArrayList <> ();
        try (StaxStream stream = new StaxStream (is)) {
            XMLStreamReader reader = stream.getReader ();
            while (stream.findElement ("group")) {
                if (!stream.findElementInScope ("group", "doc")) {
                    continue;
                }
                
                String url = "";
                if (stream.findElementInScope ("doc", "url")) {
                    url = reader.getElementText ();
                } else { continue; }
                
                String title = "";
                if (stream.findElementInScope ("doc", "title")) {
                    title = reader.getElementText ();
                } else { continue; }
                
                String headline = "";
                if (stream.findElementInScope ("doc", "headline")) {
                    headline = reader.getElementText ();
                }

                final URL link = new URL (url);
                final SActorDescriptor source = SActorDescriptor.YANDEX_ACTOR;
                rows.add (new SResponseRow (title, headline, link, source));
            }
        } catch (XMLStreamException xmlse) {
            xmlse.printStackTrace ();
        } catch (Exception e) {
            e.printStackTrace ();
        }
        
        return rows;
    }
    
}
