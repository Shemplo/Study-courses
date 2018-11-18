package ru.shemplo.estat.test;

import ru.shemplo.estat.clock.TimeProvider;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class SettableTimeProvider implements TimeProvider {

    private long time = 0;
    
    @Override
    public Long getCurrentTime () {
        return time;
    }
    
    public void setTime (long time) {
        this.time = time;
    }
    
    public void addTime (long deltatime) {
        this.time += deltatime;
    }
    
}
