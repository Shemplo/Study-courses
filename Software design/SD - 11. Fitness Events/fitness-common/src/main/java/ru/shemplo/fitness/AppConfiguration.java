package ru.shemplo.fitness;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.yaml.snakeyaml.Yaml;

import ru.shemplo.snowball.annot.PostShaped;
import ru.shemplo.snowball.annot.Snowflake;

@Snowflake
public class AppConfiguration {
    
    //@Snowflake (manual = true)
    private Map <String, Object> configuration = new HashMap <> ();
    
    @PostShaped private void readRequests () throws IOException {
        readConfigurationFile (Paths.get ("requests.yml"));
    }

    public void readConfigurationFile (Path path) throws IOException {
        try (
            InputStream is = Files.newInputStream (path);
        ) {
            readConfigurationFile (is);
        }
    }

    public void readConfigurationFile (File file) throws IOException {
        try (
            InputStream is = new FileInputStream (file);
        ) {
            readConfigurationFile (is);
        }
    }

    public void readConfigurationFile (InputStream is) throws IOException {
        Map <String, Object> config = new Yaml ().load (is);
        config.forEach (configuration::put);
    }
    
    public <T> Optional <T> get (String key) {
        @SuppressWarnings ("unchecked") T result = (T) configuration.get (key);
        return Optional.ofNullable (result);
    }
    
}
