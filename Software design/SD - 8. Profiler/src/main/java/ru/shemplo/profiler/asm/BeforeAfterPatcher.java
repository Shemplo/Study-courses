package ru.shemplo.profiler.asm;

import ru.shemplo.snowball.annot.Snowflake;

@Snowflake (priority = 0)
public class BeforeAfterPatcher implements ClassPatcher {

    @Override
    public byte [] patch (byte [] bytes) {
        return bytes;
    }
    
}
