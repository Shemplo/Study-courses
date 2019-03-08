package ru.shemplo.reactiveshop.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.db.ItemEntity;
import ru.shemplo.reactiveshop.db.ItemEntityRepository;
import ru.shemplo.reactiveshop.subjects.ShopListSubject;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListItem;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;

@Component
public class ItemsLoader implements Observer <Object> {

    @Autowired private ItemEntityRepository itemsRepository;
    @Autowired private ShopListSubject shopListSubject;
    
    @Override
    public void onNext (Object entity) {
        if (!(entity instanceof ShopListRequest)) { return; }
        
        ShopListRequest request = (ShopListRequest) entity;
        List <ItemEntity> items = itemsRepository.findAll ();
        final int total = items.size ();
        
        for (ItemEntity item : items) {
            ShopListItem listItem = new ShopListItem (item, request, total);
            shopListSubject.subject (listItem);
        }
        
        if (total == 0) {
            ShopListItem listItem = new ShopListItem (null, request, total);
            shopListSubject.subject (listItem);
        }
    }
    
    @Override
    public void onSubscribe (Disposable d) {}

    @Override
    public void onError (Throwable e) {}

    @Override
    public void onComplete () {}
    
}