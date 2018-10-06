package ru.shemplo.shop;

import ru.shemplo.shop.db.DBQuery;
import ru.shemplo.shop.db.DBQuery.CreateBuilder;
import ru.shemplo.shop.db.DBQuery.DBOperation;

public class Run {

	public static void main (String ... args) throws Exception {
		
		CreateBuilder builder = DBQuery.getBuilderFor (DBOperation.CREATE);
		System.out.println (builder.asQuery ());
		
	}
	
}
