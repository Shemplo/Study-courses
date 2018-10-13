package ru.shemplo.shop.db;

import java.sql.ResultSet;
import java.sql.SQLException;

public class FakeDBAccess extends DBAccess {
    
    public FakeDBAccess () throws SQLException {
        super ();
    }

    @Override
    public ResultSet execute (String query) throws SQLException {
        return null;
    }
    
    @Override
    public void update (String query) throws SQLException {
        
    }
    
    @Override
    public void close () throws Exception {}

}
