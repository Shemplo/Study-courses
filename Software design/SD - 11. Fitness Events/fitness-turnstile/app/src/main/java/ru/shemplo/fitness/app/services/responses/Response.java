package ru.shemplo.fitness.app.services.responses;

public interface Response <T, E extends Exception> {

    public T getResult ();

    public E getException ();

}
