package ru.shemplo.reactiveshop.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.db.CurrencyEntity;
import ru.shemplo.reactiveshop.db.CurrencyEntityRepository;
import ru.shemplo.reactiveshop.db.CurrencyQuatationsEntity;
import ru.shemplo.reactiveshop.db.CurrencyQuatationsEntityRepository;
import ru.shemplo.reactiveshop.subjects.RegisterSubject;
import ru.shemplo.reactiveshop.subjects.ShopListSubject;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserCurrency;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserRequest;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListCurrency;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;

@Service
public class CurrencyLoader implements Observer <Object> {
    
    @Autowired private CurrencyQuatationsEntityRepository currencyQuatationsEntityRepository;
    @Autowired private CurrencyEntityRepository currencyEntityRepository;
    @Autowired private RegisterSubject registerSubject;
    @Autowired private ShopListSubject shopListSubject;
    
    @Override
    public void onNext (Object entity) {
        if (entity instanceof ShopListRequest) {
            ShopListRequest request = (ShopListRequest) entity;
            List <CurrencyQuatationsEntity> quotations = currencyQuatationsEntityRepository
                                                       . findAll ();
            for (CurrencyQuatationsEntity quotation : quotations) {
                shopListSubject.subject (new ShopListCurrency (quotation, request));
            }
        } else if (entity instanceof RegisterUserRequest) {
            RegisterUserRequest request = (RegisterUserRequest) entity;
            List <CurrencyEntity> currencies = currencyEntityRepository
                                             . findAll ();
            for (CurrencyEntity currency : currencies) {
                registerSubject.subject (new RegisterUserCurrency (currency, request));
            }
        }
    }
    
    @Override
    public void onSubscribe (Disposable d) {}

    @Override
    public void onError (Throwable e) {}

    @Override
    public void onComplete () {}
    
}
