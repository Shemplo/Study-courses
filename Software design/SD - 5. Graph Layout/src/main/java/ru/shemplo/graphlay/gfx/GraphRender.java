package ru.shemplo.graphlay.gfx;

import java.awt.Color;

public interface GraphRender {

    public void strokeLine (double fx, double fy, double tx, double ty);
    
    public void strokeCircle (double x, double y, double r);
    public void fillCircle (double x, double y, double r);
    
    public void setStroke (Color color);
    public void setFill (Color color);
    
    public void setLineWidth (double width);
    
    public void clear ();
    
}
