package msqueue;


import org.junit.Test;

import com.devexperts.dxlab.lincheck.LinChecker;
import com.devexperts.dxlab.lincheck.annotations.HandleExceptionAsResult;
import com.devexperts.dxlab.lincheck.annotations.Operation;
import com.devexperts.dxlab.lincheck.annotations.Param;
import com.devexperts.dxlab.lincheck.annotations.Reset;
import com.devexperts.dxlab.lincheck.paramgen.IntGen;
import com.devexperts.dxlab.lincheck.stress.StressCTest;
import com.devexperts.dxlab.lincheck.verifier.LongExLinearizabilityVerifier;


@StressCTest
@StressCTest(iterations = 10, actorsPerThread = {"15:15", "15:15"},
    verifier = LongExLinearizabilityVerifier.class)
public class LinearizabilityTest {
    private Queue queue;

    @Reset
    public void reset() {
        queue = new MSQueue();
    }

    @Operation
    public void enqueue(@Param(gen = IntGen.class) int x) {
        queue.enqueue(x);
    }

    
    @Operation
    @HandleExceptionAsResult (EmptyQueueException.class)
    public int peek() {
        return queue.peek();
    }

    @Operation
    @HandleExceptionAsResult (EmptyQueueException.class)
    public int dequeue() {
        return queue.dequeue();
    }

    @Test
    public void test() {
        LinChecker.check(LinearizabilityTest.class);
    }
}