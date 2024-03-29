package ru.shemplo.reactiveshop.controllers;

import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.async.DeferredResult;
import org.springframework.web.servlet.ModelAndView;

import ru.shemplo.reactiveshop.db.UserEntity;
import ru.shemplo.reactiveshop.subjects.RegisterSubject;
import ru.shemplo.reactiveshop.subjects.UpdateSubject;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity;
import ru.shemplo.reactiveshop.subjects.entities.RegisterEntity.RegisterUserRequest;
import ru.shemplo.reactiveshop.subjects.entities.UpdateEntity.UpdateUserRequest;

@Controller
public class SiteController {
    
    @Autowired private RegisterSubject registerSubject;
    @Autowired private UpdateSubject updateSubject;
    
    @GetMapping ({"/", "/home"})
    public ModelAndView handleIndexPage () {
        return new ModelAndView ("index");
    }
    
    @ResponseBody 
    @PostMapping (path = "/register-user")
    public DeferredResult <String> registerUser (
                HttpServletResponse response, 
                @RequestBody String body
            ) {
        JSONObject requestBody = new JSONObject (body);

        DeferredResult <String> result = new DeferredResult <> ();
        String currency = requestBody.getString ("currency");
        String login    = requestBody.getString ("login");
        System.out.println (login + " / " + currency);
        
        RegisterEntity request = new RegisterUserRequest (result, login, currency, response);
        registerSubject.subject (request);
        
        return result;
    }
    
    @ResponseBody 
    @PostMapping (path = "/update-user")
    public DeferredResult <String> updateUser (HttpServletResponse response, 
                @RequestBody String body) {
        JSONObject requestBody = new JSONObject (body);

        DeferredResult <String> result = new DeferredResult <> ();
        String identifier   = requestBody.getString  ("identifier");
        String currency     = requestBody.getString  ("currency");
        String sorting      = requestBody.getString  ("sorting");
        String shape        = requestBody.getString  ("shape");
        String color        = requestBody.getString  ("color");
        boolean description = requestBody.getBoolean ("withDescription");
        boolean icon        = requestBody.getBoolean ("withIcon");
        
        UserEntity entity = new UserEntity (0L, null, identifier, null, icon, description, sorting, shape, color);
        UpdateUserRequest request = new UpdateUserRequest (result, entity, currency, response);
        updateSubject.subject (request);
        
        return result;
    }
    
}
