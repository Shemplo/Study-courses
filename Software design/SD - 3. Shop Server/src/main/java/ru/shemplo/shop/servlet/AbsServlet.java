package ru.shemplo.shop.servlet;

import static javax.servlet.http.HttpServletResponse.*;

import java.util.Map;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import ru.shemplo.shop.db.DBAccess;
import ru.shemplo.shop.servlet.html.HTMLBuilder;
import ru.shemplo.snowball.utils.db.DBType;

public abstract class AbsServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 447171439217987627L;

	protected final DBAccess DB = DBAccess.getInstanceOf (DBType.SQLite);
	protected final String [] REQ_PARAMS;
	protected final String SCENARIO;
	
	protected AbsServlet (String scenario, String [] requiredParams) {
		this.REQ_PARAMS = requiredParams;
		this.SCENARIO = scenario;
	}
	
	@Override
	protected final void doGet (HttpServletRequest req, HttpServletResponse resp) 
			throws ServletException, IOException {
		HTMLBuilder html = new HTMLBuilder (SCENARIO);
		resp.setContentType ("text/html");
		
		Map <String, String []> params = req.getParameterMap ();
		for (String paramName : REQ_PARAMS) {
			if (!params.containsKey (paramName)) {
				String text = "missed parameter [" + paramName + "]";
				Exception exception = new IllegalStateException (text);
				fail (resp, SC_BAD_REQUEST, html, exception);
				
				return;
			}
		}
		
		if (this instanceof RequestComputer) {
			RequestComputer computer = (RequestComputer) this;
			
			try {
				// Delegating control to child object
				computer.computeRequest (params, html);
			} catch (Exception e) {
				fail (resp, SC_INTERNAL_SERVER_ERROR, html, e);
				return;
			}
		} else {
			String text = "resuest computer not found";
			Exception exception = new IllegalStateException (text);
			fail (resp, SC_NOT_IMPLEMENTED, html, exception);
			
			return;
		}
		
		resp.getWriter ().println (html);
		resp.setStatus (SC_OK);
	}
	
	private final void fail (HttpServletResponse resp, int status, 
			HTMLBuilder html, Exception reason) throws IOException {
		html.addSmallHeader ("Request computation failed");
		html.addLine ("<b>Reason:</b>");
		
		Throwable current = reason;
		while (current.getCause () != null) {
			current = current.getCause ();
		}
		
		String name = current.getClass ().getName ();
		html.addLine ("<b>" + name + ":</b> " + current.getMessage ());
		
		resp.getWriter ().println (html);
		resp.setStatus (status);
	}

}
