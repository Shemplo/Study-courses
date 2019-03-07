package ru.shemplo.reactiveshop;

import io.reactivex.netty.RxNetty;
import ru.shemplo.reactiveshop.controllers.MainController;
import ru.shemplo.snowball.annot.Wind;
import ru.shemplo.snowball.annot.processor.Snowball;

@Wind (blow = {MainController.class})
public class RunReaciveShop extends Snowball {
    
    public static void main (String ... args) { shape (args); }
    
    private MainController mainController;
    
    @Override
    protected void onShaped (String ... args) {
        RxNetty.createHttpServer (8080, mainController::hanleRequest)
               .startAndWait     ();
    }
    
}
