package ru.shemplo.shop.servlet;

import java.util.Map;

import ru.shemplo.shop.servlet.html.HTMLBuilder;

public interface RequestComputer {

	public void computeRequest (Map <String, String []> params, HTMLBuilder html);
	
}
