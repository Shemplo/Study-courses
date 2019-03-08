package ru.shemplo.reactiveshop.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.db.UserEntity;
import ru.shemplo.reactiveshop.db.UserEntityRepository;
import ru.shemplo.reactiveshop.subjects.ShopListSubject;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListUser;

@Component
public class UsersLoader implements Observer <ShopListEntity> {

    @Autowired private UserEntityRepository userRepository;
    @Autowired private ShopListSubject shopListSubject;
    
    @Override
    public void onNext (ShopListEntity entity) {
        if (!(entity instanceof ShopListRequest)) { return; }
        
        ShopListRequest request = (ShopListRequest) entity;
        final String userID = request.getUserIdentifier ();
        
        UserEntity user = userRepository.findByIdentifier (userID);
        
        if (user != null) {
            ShopListUser listUser = new ShopListUser (user, request);
            shopListSubject.subject (listUser);
        } else {
            // TODO: change to sending error
            ShopListUser listUser = new ShopListUser (null, request);
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
