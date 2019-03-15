package ru.shemplo.reactiveshop.services;

import static ru.shemplo.snowball.utils.fp.FunctionalUtils.*;

import java.util.*;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.web.servlet.ModelAndView;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import ru.shemplo.reactiveshop.db.CurrencyEntity;
import ru.shemplo.reactiveshop.db.CurrencyQuatationsEntity;
import ru.shemplo.reactiveshop.db.ItemEntity;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListCurrency;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListItem;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListUser;
import ru.shemplo.snowball.utils.fp.FunctionalUtils.Case;

@Service
public class ShopListComposer implements Observer <Object> {

    private final Map <ShopListRequest, List <ShopListItem>> 
        items = new HashMap <> ();
    private final Map <ShopListRequest, ShopListUser> 
        users = new HashMap <> ();
    
    private final Map <ShopListRequest, Integer> 
        total = new HashMap <> ();
    
    private final Map <CurrencyEntity, CurrencyQuatationsEntity> 
        quotations = new HashMap <> ();

    @Override
    public void onNext (Object desc) {
        if (!(desc instanceof ShopListEntity)) { return; }
        ShopListRequest request = ((ShopListEntity) desc).getRequest ();
        
        switch$ (desc, 
            Case.caseOf (o -> o instanceof ShopListRequest, __ -> {
                items.putIfAbsent (request, new ArrayList <> ());
                return tryComposeResponse (request);
            }),
            
            Case.caseOf (o -> o instanceof ShopListItem, o -> (ShopListItem) o, item -> { 
                List <ShopListItem> items = this.items.get (request);
                if (item.getItem () != null) { items.add (item); }
                
                total.put (request, item.getTotal ());
                return tryComposeResponse (request);
            }),
            
            Case.caseOf (o -> o instanceof ShopListUser, o -> (ShopListUser) o, user -> {
                if (user.getUser () != null) {
                    users.put (request, user);
                } else {
                    ModelAndView view = new ModelAndView ("index");
                    request.getFuture ().setResult (view);
                }
                
                return tryComposeResponse (request);
            }),
            
            Case.caseOf (o -> o instanceof ShopListCurrency, o -> (ShopListCurrency) o, currency -> { 
                quotations.put (currency.getQuatation ().getCurrency (), currency.getQuatation ());
                return tryComposeResponse (request);
            })
        );
    }
    
    private boolean tryComposeResponse (ShopListRequest request) {
        final Integer total = this.total.get (request);
        if (total == null) { return false; }
        
        if (items.get (request).size () == total && users.get (request) != null) {
            ShopListUser user = users.get (request);
            if (!quotations.containsKey (user.getUser ().getCurrency ())) {
                return false; // currency is not actualized now
            }
            
            CurrencyEntity currency = user.getUser ().getCurrency ();
            ModelAndView view = new ModelAndView ("goods");
            view.addObject ("user", user);
            
            boolean descriptionRequired = user.getUser ().isWithDescription (),
                    iconRequired         = user.getUser ().isWithIcon ();
            
            double modifier = quotations.get (currency).getPrice ();
            List <ItemEntity> items = this.items.get (request).stream ()
                                    . map     (ShopListItem::getItem)
                                    . filter  (item -> item.getDescription () != null   || !descriptionRequired)
                                    . filter  (item -> !item.getThumbnail ().contains ("stub") || !iconRequired)
                                    . collect (Collectors.toList ());
            
            String sorting = Optional.ofNullable (user.getUser ().getSorting ()).orElse ("");
            
            switch (sorting) {
                case "alphabetASC":
                    Collections.sort (items, (a, b) -> a.getName ().compareTo (b.getName ()));
                    break;
                    
                case "alphabetDESC":
                    Collections.sort (items, (a, b) -> -a.getName ().compareTo (b.getName ()));
                    break;
                    
                case "priceASC":
                    Collections.sort (items, (a, b) -> Double.compare (a.getPrice (), b.getPrice ()));
                    break;
                    
                case "priceDESC":
                    Collections.sort (items, (a, b) -> -Double.compare (a.getPrice (), b.getPrice ()));
                    break;
                    
                case "Shuffle":
                    Collections.shuffle (items);
                    break;
                    
                default: break;
            }
            
            items.forEach (item -> {
                double price = item.getPrice () * modifier;
                double rounded = Math.round (price * 100) / 100.0;
                item.setPrice (rounded);
            });
            
            view.addObject ("items", items);
            
            List <String> currencies = this.quotations.keySet ().stream ()
                                     . map     (CurrencyEntity::getCodeISO)
                                     . collect (Collectors.toList ());
            view.addObject ("currencies", currencies);
            
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
