package ru.shemplo.actor.aggregator.engine.xml;

import java.io.InputStream;

import java.util.function.Predicate;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.events.XMLEvent;

public class StaxStream implements AutoCloseable {
    
    private static final XMLInputFactory FACTORY = XMLInputFactory.newInstance ();
    
    private final XMLStreamReader reader;
    
    public StaxStream (InputStream is) throws XMLStreamException {
        reader = FACTORY.createXMLStreamReader (is);
    }
    
    public XMLStreamReader getReader () {
        return reader;
    }
    
    public boolean findElementWithTest (String scope, String name, 
            Predicate <XMLStreamReader> test) throws XMLStreamException {
        while (reader.hasNext ()) {
            int event = reader.next ();
            
            if (event == XMLEvent.START_ELEMENT) {
                final String localName = reader.getLocalName ();
                if (localName.equals (name) && test.test (reader)) {
                    return true;
                }
            } else if (event == XMLEvent.END_ELEMENT) {
                String localName = reader.getLocalName ();
                if (localName.equals (scope)) {
                    return false;
                }
            }
            
        }
        
        return false;
    }
    
    public boolean findElementInScope (String scope, String name) throws XMLStreamException {
        return findElementWithTest (scope, name, __ -> true);
    }
    
    public boolean findElement (String name) throws XMLStreamException {
        return findElementInScope (null, name);
    }
    
    @Override
    public void close () throws Exception {
        if (reader != null) {
            reader.close ();
        }
    }
    
}
