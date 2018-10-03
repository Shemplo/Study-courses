package me.shemplo.parser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Node {

	private final List <Node> _list;
	private final String _name;
	
	public Node (String name, Node... children) {
		this._list = new ArrayList <> (Arrays.asList (children));
		this._name = name;
	}
	
	public String toString () {
		return toString ("");
	}
	
	public String toString (String prefix) {
		StringBuilder sb = new StringBuilder ();
		sb.append (String.format ("%1s\n", _name));
		
		for (Node node : _list) {
			sb.append (prefix);
			sb.append ("|- ");
			sb.append (node.toString (prefix + "   "));
		}
		
		return sb.toString ();
	}
	
}
