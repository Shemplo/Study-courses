package ru.shemplo.estat.clock;

import ru.shemplo.snowball.annot.Cooler;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class DefaultTimeProvider implements TimeProvider {

    @Cooler
    public static DefaultTimeProvider getInstance () {
        return new DefaultTimeProvider ();
    }
    
    @Override
    public Long getCurrentTime () {
        return 32L;
    }
    
}
