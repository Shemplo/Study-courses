package ru.shemplo.hw.test.implementor;

import java.io.File;
import java.io.Serializable;
import java.net.Socket;
import java.util.AbstractCollection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Future;

import ru.shemplo.hw.src.implementor.Implementor;

public class ImplementorTest {

	private static interface TA <TA0 extends Set <? extends Cloneable>> extends Runnable {
		
		public TA0 getTA0 ();
		
	}
	
	public static interface TB <TB0 extends TB1, TB1 extends Set <? extends Cloneable>> extends TA <TB0> {
		
		public TB0 getTB0 ();
		
		public TB1 getTB1 (TB1 cl, int a);
		
	}
	
	public abstract static class TCA <TC0 extends Set <? extends Cloneable>> implements TB <TC0, Set <? extends Cloneable>> {
		
		public abstract TC0 getTC0 ();
		
	}
	
	public abstract static class TCB <TCB0 extends Cloneable & Runnable & List <TCB0>> extends TCA <Set <? extends TCB0>> {

		public abstract <T extends Set <Cloneable> & TA <T>> T getT (Map <String, T> map);
		
		public abstract <R> R getR (TA <Set <? extends TCB0>> a);
		
		public void runVoid () {}
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	private static interface A extends Cloneable {
		
		@SuppressWarnings ("unused")
		default
		public void a () { /* stub */ }
		
	}
	
	@SuppressWarnings ("unused")
	private abstract static class TestClassТестовыйКласс <T extends Runnable & Future <? extends String>, 
															D extends Serializable>
		extends AbstractCollection <T> implements AutoCloseable, A {
		
		//@SuppressWarnings ("unused")
		public List <? extends Set <Socket>> getSockets () {
			return null;
		}
		
		public int getSocketsNumber () {
			return 0;
		}
		
		//@SuppressWarnings ("unused")
		public T getT () {
			return null;
		}
		
		//@SuppressWarnings ("unused")
		public int getActiveSockets (Map <Integer, ? extends Socket> sockets) {
			return getSocketsNumber () + sockets.size ();
		}
		
		//@SuppressWarnings ("unused")
		protected static <F> F checkProtected () {
			return null;
		}
		
		//@SuppressWarnings ({"unused", "unchecked"})
		@SuppressWarnings ("unchecked")
		protected void runWithVarArg (int a, int b, List <int [][]>[]... c) throws NullPointerException {
			
		}
		
	}
	
	public static void main (String... args) {
		File root = new File (".", "test");
		Implementor impl = new Implementor ();
		impl.implement (TCB.class, root.toPath ());
	}
	
}
