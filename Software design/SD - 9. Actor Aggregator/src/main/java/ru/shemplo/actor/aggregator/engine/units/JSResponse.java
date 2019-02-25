package ru.shemplo.actor.aggregator.engine.units;

import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import lombok.Data;
import lombok.NonNull;
import ru.shemplo.actor.aggregator.engine.JSActorDescriptor;

@Data
public class JSResponse {
    
    @Data
    public static class JSResponseRow {
        
        private final String title, description;
        private final URL link;
        
        private final JSActorDescriptor source;
        
    }
    
    @NonNull private Long      finishTime;
    @NonNull private Boolean   justFinished;
    private final List <JSResponseRow> rows;
    
    public JSResponse mergeIn (JSResponse response) {
        finishTime = Math.max (finishTime, response.finishTime);
        rows.addAll (response.rows);
        Collections.shuffle (rows);
        
        return this;
    }
    
    public static JSResponse empty () {
        final List <JSResponseRow> rows = new ArrayList <> ();
        return new JSResponse (0L, true, rows);
    }
    
}
