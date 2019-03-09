package ru.shemplo.reactiveshop.subjects;

import org.springframework.stereotype.Component;

import io.reactivex.Observer;
import io.reactivex.subjects.ReplaySubject;
import io.reactivex.subjects.Subject;
import ru.shemplo.reactiveshop.subjects.entities.UpdateEntity;

@Component
public class UpdateSubject implements Subjector <Object> {

    private final Subject <UpdateEntity> subject = ReplaySubject.create ();
    
    @Override
    public void subscribe (Observer <Object> observer) {
        subject.subscribe (observer);
    }

    @Override
    public void subject (Object subject) {
        this.subject.onNext ((UpdateEntity) subject);
    }
    
}
