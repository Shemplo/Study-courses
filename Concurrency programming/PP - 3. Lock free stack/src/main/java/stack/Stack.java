package main.java.stack;

import java.util.EmptyStackException;

/**
 * Stack interface.
 *
 * @author Nikita Koval
 */
public interface Stack {

    /**
     * Pushes an item onto the top of this stack.
     *
     * @param x the item to be pushed onto this stack.
     */
    void push(int x);

    /**
     * Removes the object at the top of this stack and returns that
     * object as the value of this function.
     *
     * @return The object at the top of this stack.
     * @throws EmptyStackException if this stack is empty.
     */
    int pop();
}
