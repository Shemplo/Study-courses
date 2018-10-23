package test.java.stack;

import static org.junit.Assert.assertEquals;

import java.util.Random;
import java.util.Stack;

import org.junit.Test;

import main.java.stack.StackImpl;

public class FunctionalTest {
    private static Random R = new Random(0);

    @Test
    public void test() {
        main.java.stack.Stack myStack = new StackImpl();
        Stack<Integer> javaStack = new Stack<>();
        for (int i = 0; i < 1_000_000; i++) {
            int op = R.nextInt(2);
            int key = R.nextInt(20) + 1;
            switch (op) {
            case 0:
                // push
                int x = R.nextInt();
                javaStack.push(x);
                myStack.push(x);
                break;
            case 1:
                // pop
                if (!javaStack.isEmpty()) {
                    assertEquals((int) javaStack.pop(), myStack.pop());
                }
                break;
            }
        }
    }
}