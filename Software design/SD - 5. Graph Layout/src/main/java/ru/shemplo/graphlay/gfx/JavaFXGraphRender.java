package ru.shemplo.graphlay.gfx;

import java.awt.Color;

import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Cursor;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Border;
import javafx.scene.layout.BorderStroke;
import javafx.scene.layout.BorderStrokeStyle;
import javafx.scene.layout.BorderWidths;
import javafx.scene.layout.CornerRadii;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Paint;
import javafx.stage.Stage;

import ru.shemplo.graphlay.RunGrpahLayout;

public class JavaFXGraphRender extends Application implements GraphRender {

    private static double width = 800, height = 600;
    
    public JavaFXGraphRender (double width, double height) {
        JavaFXGraphRender.height = height;
        JavaFXGraphRender.width = width; 
        
        launch ();
    }
    
    public JavaFXGraphRender () {}
    
    private volatile GraphicsContext context = null;
    private final Canvas CANVAS = new Canvas ();
    
    @Override
    public void start (Stage stage) throws Exception {
        StackPane pane = new StackPane ();
        pane.getChildren ().add (CANVAS);
        CANVAS.setHeight (height);
        CANVAS.setWidth (width);
        
        
        ImageView refreshIcon = new ImageView (new Image ("/gfx/refresh.png"));
        refreshIcon.setFitWidth (32); refreshIcon.setFitHeight (32);
        Button refresh = new Button ("", refreshIcon);
        StackPane.setAlignment (refresh, Pos.TOP_RIGHT);
        StackPane.setMargin (refresh, new Insets (15));
        pane.getChildren ().add (refresh);
        refresh.setCursor (Cursor.HAND);
        refresh.setBorder (new Border (new BorderStroke (
                Paint.valueOf ("BLACK"), BorderStrokeStyle.SOLID, 
                new CornerRadii (5), BorderWidths.DEFAULT)));
        refresh.setBackground (null);
        refresh.setOnMouseClicked (me -> {
            RunGrpahLayout.render (this);
        });
        
        pane.setMinSize (100, 100);
        pane.setOnScroll (se -> {
            double scale = Math.max (pane.getScaleX () + Math.signum (se.getDeltaY ()) * 0.05, 0.05);
            pane.setScaleX (scale); pane.setScaleY (scale);
        });
        
        Scene scene = new Scene (pane);
        stage.setTitle ("Graph plot");
        stage.setResizable (false);
        stage.setScene (scene);
        stage.sizeToScene ();
        stage.show ();
        
        context = CANVAS.getGraphicsContext2D ();
        RunGrpahLayout.onStageReady (this);
    }
    
    @Override
    public void setLineWidth (double width) {
        context.setLineWidth (width);
    }
    
    @Override
    public void strokeLine (double fx, double fy, double tx, double ty) {
        double cx = width / 2, cy = height / 2;
        context.strokeLine (cx + fx, cy + fy, cx + tx, cy + ty);
    }

    @Override
    public void strokeCircle (double x, double y, double r) {
        
    }

    @Override
    public void fillCircle (double x, double y, double r) {
        double cx = width / 2, cy = height / 2;
        context.fillOval (cx + x - r / 2, cy + y - r / 2, r, r);
    }

    @Override
    public void setStroke (Color color) {
        context.setStroke (javafx.scene.paint.Color.rgb (
            color.getRed (), color.getGreen (), 
            color.getBlue (), color.getAlpha () / 255.0
        ));
    }
    
    @Override
    public void setFill (Color color) {
        context.setFill (javafx.scene.paint.Color.rgb (
           color.getRed (), color.getGreen (), 
           color.getBlue (), color.getAlpha () / 255.0
        ));
    }

    @Override
    public void clear () {
        Paint paint = context.getFill ();
        context.setFill (javafx.scene.paint.Color.WHITE);
        context.fillRect (0, 0, width, height);
        context.setFill (paint);
    }

    
}
