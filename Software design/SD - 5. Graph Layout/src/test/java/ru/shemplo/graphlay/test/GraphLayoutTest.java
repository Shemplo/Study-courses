package ru.shemplo.graphlay.test;

import static org.junit.jupiter.api.Assertions.*;
import static ru.shemplo.graphlay.io.Parameter.*;
import static ru.shemplo.graphlay.io.ParametersData.*;

import java.awt.Color;
import java.io.IOException;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import ru.shemplo.graphlay.GraphFormat;
import ru.shemplo.graphlay.RenderType;
import ru.shemplo.graphlay.gfx.GraphRender;
import ru.shemplo.graphlay.graph.Graph;
import ru.shemplo.graphlay.io.ParametersData;

@DisplayName ("Tests for graph layout")
public class GraphLayoutTest {

    @Nested
    public static class ParametersTest {
        
        @Test
        public void testEmpty () throws IOException {
            ParametersData data = parse ("");
            assertEquals (data.toString (), "{}");
            assertEquals (data.getSize (), 0);
        }
        
        @Test
        public void testSingle () throws IOException {
            ParametersData data = parse ("-f test");
            assertEquals (data.getValue (FORMAT), "test");
        }
        
        @Test
        public void testDouble () throws IOException {
            ParametersData data = parse ("-f test -da test2");
            assertEquals (data.getValue (DRAWING_API), "test2");
            assertEquals (data.getValue (FORMAT), "test");
        }
        
        @Test
        public void testIgnored () throws IOException {
            ParametersData data = parse ("-fail fail");
            assertEquals (data.toString (), "{}");
            assertEquals (data.getSize (), 0);
        }
        
        @Test
        public void testMultiSet () throws IOException {
            ParametersData data = parse ("-fail failt -f format");
            assertEquals (data.getValue (FORMAT), "format");
            assertEquals (data.getSize (), 1);
            
            data = parse ("-fail failt -f format -fail2 fail2");
            assertEquals (data.getValue (FORMAT), "format");
            assertEquals (data.getSize (), 1);
            
            data = parse ("-fail failt -f format -fail2 fail2 -da draw");
            assertEquals (data.getValue (DRAWING_API), "draw");
            assertEquals (data.getValue (FORMAT), "format");
            assertEquals (data.getSize (), 2);
        }
        
        @Test
        public void testParameterValue () throws IOException {
            ParametersData data = parse ("-f edges");
            assertNotNull (GraphFormat.
                    matchOrDeafault (data.getValue (FORMAT), null));
            
            data = parse ("-f matrix");
            assertNotNull (GraphFormat.
                    matchOrDeafault (data.getValue (FORMAT), null));
            
            data = parse ("-f fake");
            assertNull (GraphFormat.
                    matchOrDeafault (data.getValue (FORMAT), null));
            
            data = parse ("-da awt");
            assertNotNull (RenderType.
                    matchOrDeafault (data.getValue (DRAWING_API), null));
            
            data = parse ("-da javafx");
            assertNotNull (RenderType.
                    matchOrDeafault (data.getValue (DRAWING_API), null));
            
            data = parse ("-da fake");
            assertNull (RenderType.
                    matchOrDeafault (data.getValue (DRAWING_API), null));
        }
        
    }
    
    @Nested
    public static class GraphTest {
        
        private int circles = 0, lines = 0;
        
        @Test
        public void testRenderCalls () {
            circles = 0; lines = 0;
            GraphRender render = new GraphRender() {
                
                @Override
                public void strokeLine (double fx, double fy, 
                                        double tx, double ty) {
                    lines += 1;
                }
                
                @Override
                public void strokeCircle (double x, double y, double r) {
                    circles += 1;
                }
                
                @Override
                public void fillCircle (double x, double y, double r) {
                    circles += 1;
                }
                
                @Override public void setLineWidth (double width) {}
                @Override public void setStroke (Color color) {}
                @Override public void setFill (Color color) {}
                @Override public void clear () {}
                
            };
            
            Graph graph = new Graph ();
            for (int i = 0; i < 32; i++) { graph.addVertex (i); }
            for (int i = 0; i < 31; i++) { graph.addEdge (i, i + 1); }
            graph.render (render);
            
            assertEquals (circles, 32);
            assertEquals (lines, 31);
        }
        
    }
    
}
