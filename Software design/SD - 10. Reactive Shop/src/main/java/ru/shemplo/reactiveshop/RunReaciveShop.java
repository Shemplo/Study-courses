package ru.shemplo.reactiveshop;

import java.util.concurrent.Executor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

import ru.shemplo.reactiveshop.services.*;
import ru.shemplo.reactiveshop.subjects.RegisterSubject;
import ru.shemplo.reactiveshop.subjects.ShopListSubject;

@SpringBootApplication (exclude = {SecurityAutoConfiguration.class})
public class RunReaciveShop {
    
    public static void main (String ... args) {
        Class <RunReaciveShop> mainClass = RunReaciveShop.class;
        final ConfigurableApplicationContext context 
            = SpringApplication.run (mainClass, args);
        
        context.getBean (mainClass).intinializeReactivity ();
    }
        
    @Autowired private EntitiesRegister entitiesRegister;
    @Autowired private ShopListComposer shopListComposer;
    @Autowired private CurrencyLoader currencyLoader;
    @Autowired private UsersLoader usersLoader;
    @Autowired private ItemsLoader itemsLoader;
    
    @Autowired private RegisterSubject registerSubject;
    @Autowired private ShopListSubject shopListSubject;
    
    public void intinializeReactivity () {
        shopListSubject.subscribe (shopListComposer);
        shopListSubject.subscribe (currencyLoader);
        shopListSubject.subscribe (itemsLoader);
        shopListSubject.subscribe (usersLoader);
        
        registerSubject.subscribe (entitiesRegister);
        registerSubject.subscribe (currencyLoader);
        registerSubject.subscribe (usersLoader);
    }
    
    @Configuration @EnableAsync
    public class SpringAsyncConfig implements AsyncConfigurer {
         
        @Override public Executor getAsyncExecutor () {
            ThreadPoolTaskScheduler scheduler = new ThreadPoolTaskScheduler ();
            scheduler.setPoolSize (4); scheduler.initialize ();
            return scheduler;
        }
         
    }
    
}
