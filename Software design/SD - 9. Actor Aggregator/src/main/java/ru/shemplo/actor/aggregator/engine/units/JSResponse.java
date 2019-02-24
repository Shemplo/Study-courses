package ru.shemplo.actor.aggregator.engine.units;

import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import lombok.Data;
import lombok.NonNull;

@Data
public class JSResponse {
    
    @Data
    public static class JSResponseRow {
        
        private final String title, description;
        private final URL link;
        
    }
    
    private final    JSRequest request;
    @NonNull private Long      finishTime;
    @NonNull private Boolean   justFinished;
    private final List <JSResponseRow> rows;
    
    public JSResponse mergeIn (JSResponse response) {
        rows.addAll (response.rows);
        Collections.shuffle (rows);
        
        return this;
    }
    
    public static JSResponse empty (JSRequest request) {
        final List <JSResponseRow> rows = new ArrayList <> ();
        return new JSResponse (request, 0L, false, rows);
    }
    
}
