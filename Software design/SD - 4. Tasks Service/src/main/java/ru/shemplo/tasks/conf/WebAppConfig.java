package ru.shemplo.tasks.conf;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.format.FormatterRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import ru.shemplo.tasks.db.DBAccess;

@Configuration
public class WebAppConfig implements WebMvcConfigurer {

    @Override
    public void addFormatters (FormatterRegistry registry) {
        registry.addConverter (new ToStringConverter ());
    }

    @Bean
    public DBAccess initDBAccess () {
        return new DBAccess ();
    }

}
