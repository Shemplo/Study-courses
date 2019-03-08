package ru.shemplo.reactiveshop.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.db.UserEntity;
import ru.shemplo.reactiveshop.db.UserEntityRepository;
import ru.shemplo.reactiveshop.subjects.RegisterSubject;
import ru.shemplo.reactiveshop.subjects.ShopListSubject;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserRequest;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserUser;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListUser;

@Service
public class UsersLoader implements Observer <Object> {

    @Autowired private UserEntityRepository userRepository;
    @Autowired private RegisterSubject registerSubject;
    @Autowired private ShopListSubject shopListSubject;
    
    @Override
    public void onNext (Object entity) {
        if (entity instanceof ShopListRequest) {
            ShopListRequest request = (ShopListRequest) entity;
            final String userID = request.getUserIdentifier ();
            
            UserEntity user = userRepository.findByIdentifier (userID);
            ShopListUser listUser = new ShopListUser (user, request);
            shopListSubject.subject (listUser);
        } else if (entity instanceof RegisterUserRequest) {
            RegisterUserRequest request = (RegisterUserRequest) entity;
            
            final String identifier = request.getName ().trim ().toLowerCase ();
            UserEntity user = userRepository.findByIdentifier (identifier);
            
            RegisterUserUser registerUser = new RegisterUserUser (user, request);
            registerSubject.subject (registerUser);
        }
    }
    
    @Override
    public void onSubscribe (Disposable d) {}

    @Override
    public void onError (Throwable e) {}

    @Override
    public void onComplete () {}
    
}
