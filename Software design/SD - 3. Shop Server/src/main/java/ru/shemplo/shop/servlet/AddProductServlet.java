package ru.shemplo.shop.servlet;

import java.util.Map;

import java.sql.SQLException;

import ru.shemplo.shop.db.DBLib;
import ru.shemplo.shop.servlet.html.HTMLBuilder;

public class AddProductServlet extends AbsServlet implements RequestComputer {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7511012126994740512L;
	
	public AddProductServlet () {
		super ("Add new product", new String [] {
			"name", "price"
		});
	}

	@Override
	public void computeRequest (Map <String, String []> params, HTMLBuilder html) {
		long price = Long.parseLong (params.get ("price") [0]);
		String name = params.get ("name") [0];
		
		try {
			String query = DBLib.insertProduct (name, price);
			this.DB.update (query);
			
			html.addSmallHeader ("New product added successfully");
			html.addParagraph ("<b>Product:</b> " + name + " (" 
							+ price + " c.u.)");
		} catch (SQLException sqle) { 
			throw new IllegalStateException (sqle);
		}
	}

}
