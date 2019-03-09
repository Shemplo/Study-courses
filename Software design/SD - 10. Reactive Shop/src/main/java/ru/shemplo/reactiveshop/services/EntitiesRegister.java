package ru.shemplo.reactiveshop.services;

import static ru.shemplo.snowball.utils.fp.FunctionalUtils.*;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.Cookie;
import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.db.CurrencyEntity;
import ru.shemplo.reactiveshop.db.UserEntity;
import ru.shemplo.reactiveshop.db.UserEntityRepository;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserCurrency;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserRequest;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserUser;
import ru.shemplo.reactiveshop.subjects.entities.UpdateEntity.UpdateUserCurrency;
import ru.shemplo.reactiveshop.subjects.entities.UpdateEntity.UpdateUserRequest;
import ru.shemplo.reactiveshop.subjects.entities.UpdateEntity.UpdateUserUser;
import ru.shemplo.snowball.utils.fp.FunctionalUtils.Case;

@Service
public class EntitiesRegister implements Observer <Object> {

    @Autowired private UserEntityRepository userRepository;
    
    private final Map <RegisterUserRequest, Boolean> 
        duplication = new HashMap <> ();
    
    private final Map <String, CurrencyEntity> 
        currencies = new HashMap <> ();
    
    @Override
    public void onNext (Object entity) {
        switch$ (entity, 
            Case.caseOf (o -> o instanceof RegisterUserRequest, 
                o -> (RegisterUserRequest) o, this::tryRegisterUser),
            
            Case.caseOf (o -> o instanceof RegisterUserCurrency, 
                    o -> (RegisterUserCurrency) o, 
                    cur -> {
                        currencies.put (cur.getCurrency ().getCodeISO (), cur.getCurrency ());
                        return tryRegisterUser (cur.getRequest ());
                    }),
            
            Case.caseOf (o -> o instanceof RegisterUserUser, 
                    o -> (RegisterUserUser) o, 
                    user -> {
                        duplication.put (user.getRequest (), user.getUser () != null);
                        return tryRegisterUser (user.getRequest ());
                    }),
            
            Case.caseOf (o -> o instanceof UpdateUserUser, 
                o -> (UpdateUserUser) o, user -> tryUpdateUser (user.getRequest ())),
            
            Case.caseOf (o -> o instanceof UpdateUserCurrency, 
                    o -> (UpdateUserCurrency) o, 
                    cur -> {
                        currencies.put (cur.getCurrency ().getCodeISO (), cur.getCurrency ());
                        return tryUpdateUser (cur.getRequest ());
                    })
        );
    }
    
    @Transactional
    private boolean tryRegisterUser (RegisterUserRequest request) {
        if (currencies.containsKey (request.getCurrency ()) 
                && !request.getFuture ().isSetOrExpired ()
                && duplication.containsKey (request)) {
            String identifier = request.getName ().trim ().toLowerCase ();
            
            if (!duplication.get (request)) {
                CurrencyEntity currency = currencies.get (request.getCurrency ());
                UserEntity user = UserEntity.builder ()
                                . identifier (identifier)
                                . login      (request.getName ())
                                . currency   (currency)
                                . build      ();
                userRepository.save (user);
            }
            
            request.getResponse ().addCookie (new Cookie ("Client", identifier));
            request.getFuture ().setResult ("registered");
            return true;
        }
        
        return false;
    }
    
    @Transactional
    private boolean tryUpdateUser (UpdateUserRequest request) {
        if (currencies.containsKey (request.getCurrency ()) && !request.getFuture ().isSetOrExpired ()
                && request.getUpdatedUser ().getId () != 0L) {
            request.getUpdatedUser ().setCurrency (currencies.get (request.getCurrency ()));
            userRepository.deleteById (request.getUpdatedUser ().getId ());
            userRepository.save (request.getUpdatedUser ());
            
            final String identifier = request.getUpdatedUser ().getIdentifier ();
            request.getResponse ().addCookie (new Cookie ("Client", identifier));
            request.getFuture ().setResult ("updated");
            return true;
        }
        
        return false;
    }
    
    @Override
    public void onSubscribe (Disposable d) {}

    @Override
    public void onError (Throwable e) {}

    @Override
    public void onComplete () {}
    
}
