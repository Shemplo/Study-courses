package skip_list;

import org.junit.Test;

import java.util.HashSet;
import java.util.Random;

import static org.junit.Assert.assertEquals;

public class FunctionalTest {
    private static Random R = new Random(0);

    @Test
    public void test() {
        Set skipList = new SkipList();
        java.util.Set<Integer> javaSet = new HashSet<>();
        for (int i = 0; i < 1_000_000; i++) {
            int op = R.nextInt(3);
            int x = R.nextInt(30);
            switch (op) {
            case 0:
                // add
                assertEquals(javaSet.add(x), skipList.add(x));
                break;
            case 1:
                // contains
                assertEquals(javaSet.contains(x), skipList.contains(x));
                break;
            case 2:
                // remove
                assertEquals(javaSet.remove(x), skipList.remove(x));
                break;
            }
        }
    }
}