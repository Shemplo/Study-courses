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
import javax.xml.stream.events.XMLEvent;

import ru.shemplo.actor.aggregator.engine.units.JSRequest;
import ru.shemplo.actor.aggregator.engine.units.JSResponse.JSResponseRow;
import ru.shemplo.actor.aggregator.engine.xml.StaxStream;

public class YandexJSActor extends AbsJSActor {
    
    @Override
    protected URL makeGetRequestURL (JSRequest request) throws RuntimeException {
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
    protected List <JSResponseRow> parseResponse (String response) {
        final byte [] bytes = response.getBytes (StandardCharsets.UTF_8);
        InputStream is = new ByteArrayInputStream (bytes);
        
        List <JSResponseRow> rows = new ArrayList <> ();
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
                    title = readTextMaybeWithTags (reader);
                } else { continue; }
                
                String headline = "";
                if (stream.findElementInScope ("doc", "headline")) {
                    headline = readTextMaybeWithTags (reader);
                }

                final URL link = new URL (url);
                final JSActorDescriptor source = JSActorDescriptor.YANDEX_ACTOR;
                rows.add (new JSResponseRow (title, headline, link, source));
            }
        } catch (XMLStreamException xmlse) {
            xmlse.printStackTrace ();
        } catch (Exception e) {
            e.printStackTrace ();
        }
        
        return rows;
    }
    
    private String readTextMaybeWithTags (XMLStreamReader reader) throws XMLStreamException {
        String value = "";
        
        int event = reader.next ();
        if (event == XMLEvent.START_ELEMENT) {
            value = reader.getElementText ();
            event = reader.next (); // out from </hlword>
            if (event == XMLEvent.CHARACTERS) {
                value.concat (reader.getText ());
            }
        } else {
            value = reader.getText ();
        }
        
        return value;
    }
    
}
