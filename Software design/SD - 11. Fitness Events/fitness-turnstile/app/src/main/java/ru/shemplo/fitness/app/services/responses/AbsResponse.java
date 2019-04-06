package ru.shemplo.fitness.app.services.responses;

import android.util.Log;

public abstract class AbsResponse <T, E extends Exception> implements Response <T, E> {

    protected final E exception;
    protected final T value;

    public AbsResponse (final T value, final E exception) {
        this.value = value; this.exception = exception;
    }

    @Override
    public T getResult () { return value; }

    @Override
    public E getException () { return exception; }
}
