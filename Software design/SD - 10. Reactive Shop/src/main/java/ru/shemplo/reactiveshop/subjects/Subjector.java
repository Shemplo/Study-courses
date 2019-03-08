package ru.shemplo.reactiveshop.subjects;

import io.reactivex.Observer;

public interface Subjector <T> {
    
    public void subscribe (Observer <T> observer);
    
    public void subject (T subject);
    
}
