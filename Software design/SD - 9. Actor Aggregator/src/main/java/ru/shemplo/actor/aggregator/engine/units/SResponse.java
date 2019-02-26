package ru.shemplo.actor.aggregator.engine.units;

import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import lombok.Data;
import lombok.NonNull;
import ru.shemplo.actor.aggregator.engine.SActorDescriptor;

@Data
public class SResponse {
    
    @Data
    public static class SResponseRow {
        
        private final String title, description;
        private final URL link;
        
        private final SActorDescriptor source;
        
    }
    
    @NonNull private Long      finishTime;
    @NonNull private Boolean   justFinished;
    private final List <SResponseRow> rows;
    
    private long duration = 0l;
    
    public SResponse mergeIn (SResponse response) {
        finishTime = Math.max (finishTime, response.finishTime);
        rows.addAll (response.rows);
        Collections.shuffle (rows);
        
        return this;
    }
    
    public static SResponse empty () {
        final List <SResponseRow> rows = new ArrayList <> ();
        return new SResponse (0L, true, rows);
    }
    
}
