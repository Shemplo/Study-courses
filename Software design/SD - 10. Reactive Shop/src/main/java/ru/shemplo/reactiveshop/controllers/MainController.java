package ru.shemplo.reactiveshop.controllers;

import io.netty.buffer.ByteBuf;
import io.reactivex.netty.protocol.http.server.HttpServerRequest;
import io.reactivex.netty.protocol.http.server.HttpServerResponse;
import lombok.Getter;
import ru.shemplo.reactiveshop.RequestDesc;
import ru.shemplo.snowball.annot.Snowflake;
import rx.Observable;
import rx.subjects.PublishSubject;

@Snowflake
public class MainController {
    
    @Getter private final PublishSubject <RequestDesc> 
        requests = PublishSubject.create ();
    
    private ShopListController shopListController;
    
    public Observable <Void> hanleRequest (HttpServerRequest <ByteBuf> req, 
            HttpServerResponse <ByteBuf> resp) {
        RequestDesc request = new RequestDesc (req, resp);
        return shopListController.handle (request);
    }
    
}
