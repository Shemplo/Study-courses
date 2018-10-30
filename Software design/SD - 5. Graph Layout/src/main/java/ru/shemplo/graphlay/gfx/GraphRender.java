package ru.shemplo.graphlay.gfx;

public interface GraphRender {

    public void strokeLine (double fx, double fy, double tx, double ty);
    
    public void strokeCircle (double x, double y, double r);
    public void fillCircle (double x, double y, double r);
    
}
