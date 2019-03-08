package ru.shemplo.reactiveshop.services;

import static ru.shemplo.snowball.utils.fp.FunctionalUtils.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Component;
import org.springframework.web.servlet.ModelAndView;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListItem;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListUser;
import ru.shemplo.snowball.utils.fp.FunctionalUtils.Case;

@Component
public class ShopListComposer implements Observer <ShopListEntity> {

    private final Map <ShopListRequest, List <ShopListItem>> 
        items = new HashMap <> ();
    private final Map <ShopListRequest, ShopListUser> 
        users = new HashMap <> ();
    
    private final Map <ShopListRequest, Integer> 
        total = new HashMap <> ();

    @Override
    public void onNext (ShopListEntity desc) {        
        ShopListRequest request = desc.getRequest ();
        
        switch$ (desc, 
            Case.caseOf (o -> o instanceof ShopListRequest, __ -> {
                items.putIfAbsent (request, new ArrayList <> ());
                System.out.println (__);
                return false;
            }),
            
            Case.caseOf (o -> o instanceof ShopListItem, o -> (ShopListItem) o, 
                    item -> { 
                System.out.println (item);
                List <ShopListItem> items = this.items.get (request);
                if (item.getItem () != null) { items.add (item); }
                
                total.put (request, item.getTotal ());
                return tryComposeResponse (request);
            }),
            
            Case.caseOf (o -> o instanceof ShopListUser, o -> (ShopListUser) o, 
                    user -> {
                System.out.println (user);
                if (user.getUser () != null) {
                    users.put (request, user);
                } else {
                    ModelAndView view = new ModelAndView ("index");
                    request.getFuture ().setResult (view);
                }
                
                return tryComposeResponse (request);
            })
        );
    }
    
    private boolean tryComposeResponse (ShopListRequest request) {
        final Integer total = this.total.get (request);
        if (total == null) { return false; }
        
        if (items.get (request).size () == total && users.get (request) != null) {
            ModelAndView view = new ModelAndView ("goods");
            
            ShopListUser user = users.get (request);
            view.addObject ("user", user);
            
            double modifier = user.getCurrencyModifier ();
            List <ShopListItem> items = this.items.get (request);
            items.forEach (item -> {
                double price = item.getItem ().getPrice () * modifier;
                double rounded = Math.round (price * 100) / 100.0;
                item.getItem ().setPrice (rounded);
            });
            view.addObject ("items", items);
            
            this.items.remove (request);
            this.total.remove (request);
            users.remove (request);
            
            request.getFuture ().setResult (view);
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
