<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri = "http://java.sun.com/jsp/jstl/functions" prefix = "fn" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<!DOCTYPE HTML>
<html>
    <head>
        <meta charset="UTF-8" />
        <title>Shop :: goods</title>
        
        <!--<link rel="stylesheet" href="/resources/css/index.css" />-->
        <!--<link rel="shortcut icon" href="/resources/gfx/jiraf.png" type="image/x-icon">-->
    </head>
    
    <body>
        <h3>Hi, ${user.getUser ().getLogin ()}</h3>
        
        <c:forEach var="item" items="${items}">
            <div>
                ${item.getItem ().getName ()} - ${item.getItem ().getPrice ()}
            </div>
        </c:forEach>
    </body>
</html>