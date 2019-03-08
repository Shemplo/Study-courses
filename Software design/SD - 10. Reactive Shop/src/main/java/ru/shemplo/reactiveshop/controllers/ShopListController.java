package ru.shemplo.reactiveshop.controllers;

import java.util.Arrays;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.context.request.async.DeferredResult;
import org.springframework.web.servlet.ModelAndView;

import ru.shemplo.reactiveshop.subjects.ShopListSubject;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity.ShopListRequest;

@Controller
public class ShopListController {
    
    @Autowired private ShopListSubject shopListSubject;
    
    @GetMapping (path = {"/goods"})
    public DeferredResult <ModelAndView> handleShopList (HttpServletRequest request) {
        Cookie cookie = Arrays.asList (request.getCookies ()).stream ()
                      . filter    (c -> c.getName ().equals ("Client"))
                      . findFirst ().orElse (new Cookie ("Client", "guest"));
        
        DeferredResult <ModelAndView> result = new DeferredResult <> ();
        ShopListEntity desc = new ShopListRequest (result, cookie.getValue ());
        shopListSubject.subject (desc);
        
        return result;
    }
    
}
