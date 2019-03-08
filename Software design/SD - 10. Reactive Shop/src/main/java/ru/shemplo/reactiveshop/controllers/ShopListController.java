package ru.shemplo.reactiveshop.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.context.request.async.DeferredResult;
import org.springframework.web.servlet.ModelAndView;

import ru.shemplo.reactiveshop.subjects.ShopListSubject;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;

@Controller
public class ShopListController {
    
    @Autowired private ShopListSubject shopListSubject;
    
    @GetMapping ("/goods")
    public DeferredResult <ModelAndView> handleShopListForGuest () {
        return handleShopList ("guest");
    }
    
    @GetMapping (path = {"/goods"}, params = {"user"})
    public DeferredResult <ModelAndView> handleShopList (
                @RequestParam ("user") final String user
            ) {
        DeferredResult <ModelAndView> result = new DeferredResult <> ();
        ShopListEntity desc = new ShopListRequest (result, user);
        shopListSubject.subject (desc);
        
        return result;
    }
    
    @GetMapping ("/")
    public ModelAndView handleShopList2 () {
        ModelAndView view = new ModelAndView ("index");
        return view;
    }
    
}
