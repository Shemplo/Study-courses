package ru.shemplo.shop.db;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public class DBQuery {

	/**
	 * ...
	 * 
	 * @author Shemp
	 *
	 */
	public static enum DBOperation {
		
		CREATE (CreateBuilder.class),
		
		SELECT (null), 
		
		INSERT (null);
		
		private final Class <? extends Builder> TYPE_TOKEN;
		
		public <R extends Builder> R getBuilder () throws NoSuchMethodException {
			try {
				if (TYPE_TOKEN == null) { throw new NoSuchMethodException (); }
				
				@SuppressWarnings ("unchecked")
				Constructor <R> constructor = (Constructor <R>) TYPE_TOKEN
											. getConstructor ();
				return constructor.newInstance ();
			} catch (InvocationTargetException | IllegalAccessException 
					 | InstantiationException ies) {
				return null;
			}
		}
		
		private DBOperation (Class <? extends Builder> typeToken) {
			this.TYPE_TOKEN = typeToken;
		}
		
	}
	
	public static <R extends Builder> R getBuilderFor (DBOperation operation) 
			throws NoSuchMethodException {
		return operation.getBuilder ();
	}
	
	/**
	 * 
	 * ...
	 * 
	 * @author Shemp
	 *
	 */
	public abstract static class Builder {
		
		public abstract DBQuery asQuery ();
		
	}
	
	///////////////////////////////////////
	
	public static class CreateBuilder extends Builder {
		
		@Override
		public DBQuery asQuery () {
			return null;
		}
		
	}
	
}
