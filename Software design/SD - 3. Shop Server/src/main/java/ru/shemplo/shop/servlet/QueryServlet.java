package ru.shemplo.shop.servlet;

import java.util.Map;

import ru.shemplo.shop.servlet.html.HTMLBuilder;
import ru.shemplo.snowball.stuctures.Pair;

public class QueryServlet extends AbsServlet implements RequestComputer {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4442168039401537019L;

	public QueryServlet () {
		super ("Query execution result", new String [] {
			"command"
		});
	}

	@Override
	public void computeRequest (Map <String, String []> params, HTMLBuilder html) {
		String command = params.get ("command") [0].toUpperCase ();
		RequestCommand en = RequestCommand.valueOf (command);
		en.C.accept (Pair.mp (this.DB, html));
	}
	
}
