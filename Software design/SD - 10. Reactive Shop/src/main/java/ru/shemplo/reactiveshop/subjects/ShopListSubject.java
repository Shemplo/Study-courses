package ru.shemplo.reactiveshop.subjects;

import org.springframework.stereotype.Component;

import io.reactivex.Observer;
import io.reactivex.subjects.ReplaySubject;
import io.reactivex.subjects.Subject;
import ru.shemplo.reactiveshop.subjects.entities.ShopListEntity;

@Component
public class ShopListSubject implements Subjector <ShopListEntity> {

    private final Subject <ShopListEntity> subject = ReplaySubject.create ();
    
    @Override
    public void subscribe (Observer <ShopListEntity> observer) {
        subject.subscribe (observer);
    }
    
    @Override
    public void subject (ShopListEntity subject) {
        this.subject.onNext (subject);
    }
    
}
