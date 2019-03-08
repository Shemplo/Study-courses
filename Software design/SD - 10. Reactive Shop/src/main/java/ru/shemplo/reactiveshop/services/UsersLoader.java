package ru.shemplo.reactiveshop.services;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.db.*;
import ru.shemplo.reactiveshop.subjects.ShopListSubject;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListUser;

@Component
public class UsersLoader implements Observer <ShopListEntity> {

    @Autowired private CurrencyQuatationsEntityRepository currencyQuatationsEntityRepository;
    @Autowired private CurrencyEntityRepository currencyEntityRepository;
    
    @Autowired private UserEntityRepository userRepository;
    @Autowired private ShopListSubject shopListSubject;
    
    private CurrencyEntity USD;
    
    @PostConstruct
    private void init () {
        USD = currencyEntityRepository.findByCodeISO ("USD");
    }
    
    @Override
    public void onNext (ShopListEntity entity) {
        if (!(entity instanceof ShopListRequest)) { return; }
        
        ShopListRequest request = (ShopListRequest) entity;
        final String userID = request.getUserIdentifier ();
        
        UserEntity user = userRepository.findByIdentifier (userID);
        
        if (user != null) {
            CurrencyQuatationsEntity userCurrency = currencyQuatationsEntityRepository
                                                  . findLastByCurrency (user.getCurrency ());
            CurrencyQuatationsEntity usdCurrency  = currencyQuatationsEntityRepository
                                                  . findLastByCurrency (USD);
            System.out.println (userCurrency);
            System.out.println (usdCurrency);
            
            double modifier = userCurrency.getPrice () / usdCurrency.getPrice ();
            ShopListUser listUser = new ShopListUser (user, modifier, request);
            shopListSubject.subject (listUser);
        } else {
            // TODO: change to sending error
            ShopListUser listUser = new ShopListUser (null, 1.0, request);
            shopListSubject.subject (listUser);
        }
    }
    
    @Override
    public void onSubscribe (Disposable d) {}

    @Override
    public void onError (Throwable e) {}

    @Override
    public void onComplete () {}
    
}
