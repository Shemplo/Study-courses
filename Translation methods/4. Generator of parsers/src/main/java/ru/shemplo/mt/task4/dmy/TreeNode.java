package ru.shemplo.mt.task4.dmy;

import java.util.ArrayList;
import java.util.List;

public class TreeNode {
	
	public final List <TreeNode> CHILDREN = new ArrayList <> ();
	
	public String value;
	
	public TreeNode (String value) {
		this.value = value;
	}
	
	@Override
	public String toString () {
		StringBuilder sb = new StringBuilder (value);
		if (CHILDREN.size () > 0) {
			sb.append (" ")
			  .append (CHILDREN.toString ());
		}
		
			return sb.toString ();
	}
	
	public void printTree (String prefix) {
		System.out.println (prefix + value.trim ());
		for (TreeNode child : CHILDREN) {
			child.printTree (prefix + "| ");
		}
	}
	
}
