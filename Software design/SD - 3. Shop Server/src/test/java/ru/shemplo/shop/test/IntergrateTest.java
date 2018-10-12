package ru.shemplo.shop.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import java.io.File;
import java.sql.ResultSet;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

import ru.shemplo.shop.db.DBAccess;
import ru.shemplo.shop.db.DBLib;
import ru.shemplo.shop.servlet.AddProductServlet;
import ru.shemplo.shop.servlet.GetProductsServlet;
import ru.shemplo.shop.servlet.QueryServlet;
import ru.shemplo.shop.servlet.html.HTMLBuilder;
import ru.shemplo.snowball.utils.db.DBType;

public class IntergrateTest {

    public static final Random RANDOM = new Random ();
    public static final String TEMP_FILE = "temp.db";
    
    @BeforeAll
    public static void init () {
        System.setProperty ("shop.db.url", TEMP_FILE);
    }
    
    @Nested
    public class TestDB {
        
        @Test
        public void testConnection () throws Exception {
            DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
            db.close ();
        }
        
        @RepeatedTest (2)
        public void testCreateTable () throws Exception {
            DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
            db.update (DBLib.createTable ());
            
            // Repeated action to check stability on existing table
            db.update (DBLib.createTable ());
            
            // Clearing database for any cases
            db.update ("DROP TABLE `products`");
            db.close ();
        }
        
        @RepeatedTest (2)
        public void testCountRows () throws Exception {
            DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
            db.update (DBLib.createTable ());
            
            int rows = 10 + RANDOM.nextInt (27);
            for (int i = 1; i <= rows; i++) {
                db.update (DBLib.insertProduct ("test" + i, i + 1));
            }
            
            ResultSet result = db.execute (DBLib.selectAggregation ("COUNT", "*"));
            assertEquals ("" + rows, result.getString ("result"));
            db.close ();
            
            db = DBAccess.getInstanceOf (DBType.SQLite);
            // Clearing database for any cases
            db.update ("DROP TABLE `products`");
            db.close ();
        }
        
        @Test
        public void testGetMaxAndMin () throws Exception {
            DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
            db.update (DBLib.createTable ());
            
            int rows = 10 + RANDOM.nextInt (24);
            for (int i = 1; i <= rows; i++) {
                db.update (DBLib.insertProduct ("test" + i, i + 1));
            }
            
            ResultSet result = db.execute (DBLib.selectOrderedColumn ("price", "DESC", true));
            assertEquals ("test" + rows, result.getString ("name"));
            
            result = db.execute (DBLib.selectOrderedColumn ("price", "ASC", true));
            assertEquals ("test1", result.getString ("name"));
            db.close ();
            
            db = DBAccess.getInstanceOf (DBType.SQLite);
            // Clearing database for any cases
            db.update ("DROP TABLE `products`");
            db.close ();
        }
        
    }
    
    @Nested
    public class TestServlets {
        
        @Test
        public void testAddProduct () throws Exception {
            AddProductServlet add = new AddProductServlet ();
            
            DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
            db.update (DBLib.createTable ());
            
            int rows = 5 + RANDOM.nextInt (23), 
                defaultLength = new HTMLBuilder ("test")
                              . toString ().length ();
            
            for (int i = 0; i < rows; i++) {
                Map <String, String []> params = new HashMap <> ();
                params.put ("name", new String [] {"test" + i});
                params.put ("price", new String [] {"" + (i + 1)});
                
                HTMLBuilder builder = new HTMLBuilder ("test");
                add.computeRequest (params, builder);
                
                assertTrue (builder.toString ().length () > defaultLength);
            }
            
            ResultSet result = db.execute (DBLib.selectAggregation ("COUNT", "*"));
            assertEquals ("" + rows, result.getString ("result"));
            db.close ();
        }
        
        @Test
        public void testGetProducts () throws Exception {
            testAddProduct ();
            
            GetProductsServlet get = new GetProductsServlet ();
            HTMLBuilder builder = new HTMLBuilder ("test");
            
            get.computeRequest (new HashMap <> (), builder);
            assertTrue (builder.toString ().indexOf ("table") != -1);
            
            DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
            // Clearing database for any cases
            db.update ("DROP TABLE `products`");
            db.close ();
        }
        
        @Test
        public void testQuery () throws Exception {
            testAddProduct ();
            
            QueryServlet query = new QueryServlet ();
            HTMLBuilder builder = new HTMLBuilder ("page title");
            
            Map <String, String []> params = new HashMap <> ();
            params.put ("command", new String [] {"min"});
            
            query.computeRequest (params, builder);
            
            String html = builder.toString ();
            assertTrue (html.indexOf ("test0") != -1);
            assertTrue (html.indexOf ("1") != -1);
            
            DBAccess db = DBAccess.getInstanceOf (DBType.SQLite);
            // Clearing database for any cases
            db.update ("DROP TABLE `products`");
            db.close ();
        }
        
    }
    
    @AfterAll
    public static void clear () {
        File db = new File (TEMP_FILE);
        if (db.exists ()) {
            db.delete ();
        }
    }
    
}
