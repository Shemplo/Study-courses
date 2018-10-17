package ru.shemplo.tasks.conf;

import org.springframework.core.convert.converter.Converter;

public class ToStringConverter implements Converter <Object, String> {

    @Override
    public String convert (Object source) {
        return source.toString ();
    }
    
}
