package ru.shemplo.fitness;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import java.util.Map;
import java.util.Optional;

import org.yaml.snakeyaml.Yaml;

import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class AppConfiguration {
    
    @Snowflake (manual = true)
    private Map <String, Object> configuration;
    
    public void readConfigurationFile (Path path) throws IOException {
        try (
            InputStream is = Files.newInputStream (path);
        ) {            
            configuration = new Yaml ().load (is);
        }
    }
    
    public <T> Optional <T> get (String key) {
        @SuppressWarnings ("unchecked") T result = (T) configuration.get (key);
        return Optional.ofNullable (result);
    }
    
}
