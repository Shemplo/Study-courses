package ru.shemplo.tasks.conf;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import ru.shemplo.tasks.db.DBAccess;

@Configuration
public class WebAppConfig {

    /*
    @Bean
    public InternalResourceViewResolver viewResolver () {
        return new InternalResourceViewResolver ("WEB-INF/jsp/", ".jsp");
    }
    */
    
    @Bean
    public DBAccess initDBAccess () {
        return new DBAccess ();
    }
    
}
