package ru.shemplo.reactiveshop.subjects.entities;

import javax.servlet.http.HttpServletResponse;

import org.springframework.web.context.request.async.DeferredResult;

import lombok.*;
import ru.shemplo.reactiveshop.db.CurrencyEntity;
import ru.shemplo.reactiveshop.db.UserEntity;

public abstract class UpdateEntity {
    
    public abstract UpdateRequest getRequest ();
    
    @ToString @Getter @Setter
    @RequiredArgsConstructor @EqualsAndHashCode (callSuper = true)
    public static abstract class UpdateRequest extends UpdateEntity {
        
        protected final DeferredResult <String> future;
        
    }
    
    @ToString @Getter @Setter
    @EqualsAndHashCode (callSuper = true)
    public static class UpdateUserRequest extends UpdateRequest {
        
        private final HttpServletResponse response;
        
        private final UserEntity updatedUser;
        private final String currency;
        
        public UpdateUserRequest (DeferredResult <String> future, UserEntity user, 
                String currency, HttpServletResponse response) {
            super (future);
            this.updatedUser = user; this.currency = currency;
            this.response = response;
        }
        
        @Override
        public UpdateRequest getRequest () { return this; }
        
    }
    
    @ToString @Getter @Setter
    @RequiredArgsConstructor @EqualsAndHashCode (callSuper = true)
    public static class UpdateUserCurrency extends UpdateEntity {
        
        private final CurrencyEntity currency;
        
        private final UpdateUserRequest request;
        
    }
    
    @ToString @Getter @Setter
    @RequiredArgsConstructor @EqualsAndHashCode (callSuper = true)
    public static class UpdateUserUser extends UpdateEntity {
        
        private final UserEntity user;
        
        private final UpdateUserRequest request;
        
    }
    
}
