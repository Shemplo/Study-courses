package ru.shemplo.reactiveshop;

import io.netty.buffer.ByteBuf;
import io.reactivex.netty.protocol.http.server.HttpServerRequest;
import io.reactivex.netty.protocol.http.server.HttpServerResponse;
import ru.shemplo.snowball.stuctures.Pair;

public class RequestDesc extends Pair <HttpServerRequest <ByteBuf>, HttpServerResponse <ByteBuf>> {

    private static final long serialVersionUID = 2759522826847657510L;

    public RequestDesc (HttpServerRequest <ByteBuf> F, 
            HttpServerResponse <ByteBuf> S) {
        super (F, S);
    }
    
}
