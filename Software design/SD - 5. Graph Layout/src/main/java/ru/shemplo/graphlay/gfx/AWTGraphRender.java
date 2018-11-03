package ru.shemplo.graphlay.gfx;

import static java.awt.BasicStroke.*;

import java.awt.BasicStroke;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.BufferStrategy;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.border.EmptyBorder;

import ru.shemplo.graphlay.RunGrpahLayout;

public class AWTGraphRender implements GraphRender {

    private final Canvas CANVAS = new Canvas ();
    private double width, height;
    private BufferStrategy bs;
    private Graphics g;
    
    public AWTGraphRender (int width, int height) {
        this.width = width; this.height = height;
        
        Toolkit toolkit = Toolkit.getDefaultToolkit ();
        Dimension screenD = toolkit.getScreenSize ();
        
        JFrame frame = new JFrame ("Graph plot");
        frame.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE);
        
        frame.setLocation ((screenD.width - width) / 2, 
                           (screenD.height - height) / 2);
        frame.setSize (width, height);
        frame.setResizable (false);
        frame.setVisible (true);
        
        JLayeredPane panel = new JLayeredPane ();
        panel.setLayout (null);
        
        CANVAS.setSize (width, height);
        panel.add (CANVAS, 1);
        
        Image image = null;
        try {
            image = ImageIO.read (
                this.getClass ().getResource ("/gfx/refresh.png")
            );
            image = image.getScaledInstance (32, 32, 
                        Image.SCALE_SMOOTH);
        } catch (IOException ioe) {}
        
        JButton refresh = new JButton ();
        refresh.setBackground (new Color (0f, 0f, 0f, 0f));
        refresh.setBorder (new EmptyBorder (5, 5, 5, 5));
        refresh.setBorderPainted (false);
        refresh.setFocusPainted (false);
        if (image != null) {
            refresh.setIcon (new ImageIcon (image));
            refresh.setSize (32, 32);
        } else {
            refresh.setText ("Refresh");
            refresh.setSize (96, 32);
        }
        refresh.setLocation (10, 10);
        refresh.addActionListener (ae -> {
            RunGrpahLayout.render (this);
        });
        panel.add (refresh, 0);
        
        frame.add (panel);
        frame.setVisible (true);
        
        CANVAS.createBufferStrategy (3);
        bs = CANVAS.getBufferStrategy ();
        g = bs.getDrawGraphics ();
        
        try { // initialization of graphics
            Thread.sleep (150);
        } catch (InterruptedException e) {
            return;
        }
        
        RunGrpahLayout.onStageReady (this);
        //g.dispose ();
        bs.show ();
    }
    
    @Override
    public void strokeLine (double fx, double fy, double tx, double ty) {
        double cx = width / 2, cy = height / 2;
        g.drawLine ((int) (cx + fx), (int) (cy + fy), 
                    (int) (cx + tx), (int) (cy + ty));
        bs.show ();
    }

    @Override
    public void strokeCircle (double x, double y, double r) {
        double cx = width / 2, cy = height / 2;
        int ix = (int) (cx + x - r / 2), 
            iy = (int) (cy + y - r / 2);
        g.drawOval (ix, iy, (int) r, (int) r);
        bs.show ();
    }

    @Override
    public void fillCircle (double x, double y, double r) {
        double cx = width / 2, cy = height / 2;
        int ix = (int) (cx + x - r / 2), 
            iy = (int) (cy + y - r / 2);
        g.fillOval (ix, iy, (int) r, (int) r);
        bs.show ();
    }

    @Override
    public void setStroke (Color color) {
        g.setColor (color);
        bs.show ();
    }

    @Override
    public void setFill (Color color) {
        g.setColor (color);
        bs.show ();
    }

    @Override
    public void setLineWidth (double width) {
        float w = (float) width;
        ((Graphics2D) g).setStroke (new BasicStroke (w, CAP_ROUND, JOIN_ROUND));
        bs.show ();
    }

    @Override
    public void clear () {
        g.clearRect (0, 0, (int) width, (int) height);
    }

}
