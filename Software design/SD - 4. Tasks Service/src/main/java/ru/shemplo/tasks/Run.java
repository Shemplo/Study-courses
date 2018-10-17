package ru.shemplo.tasks;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Import;

import ru.shemplo.tasks.conf.WebAppConfig;

@Import (WebAppConfig.class)
@SpringBootApplication
public class Run {

    static {
        PropertiesLoader.load ("src/main/resources/application.properties");
    }
    
    public static void main (String ... args) {
        SpringApplication.run (Run.class, args);
    }
    
}
