package ru.shemplo.reactiveshop.controllers;

import io.netty.handler.codec.http.HttpResponseStatus;
import ru.shemplo.reactiveshop.RequestDesc;
import ru.shemplo.snowball.annot.PostShaped;
import ru.shemplo.snowball.annot.Snowflake;
import rx.Observable;

@Snowflake
public class ShopListController {
    
    //private MainController mainController;
    
    @PostShaped private void init () {
        System.out.println ("Post shaped");
    }

    public Observable <Void> handle (RequestDesc request) {
        request.S.setStatus (HttpResponseStatus.OK);
        request.S.writeString ("It works");
        return request.S.close ();
    }
    
}
