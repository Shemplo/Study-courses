<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri = "http://java.sun.com/jsp/jstl/functions" prefix = "fn" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<!DOCTYPE HTML>
<html>
    <head>
        <meta charset="UTF-8" />
        <title>JiraF organizer</title>
        
        <link rel="stylesheet" href="/resources/css/error.css" />
        <link rel="shortcut icon" href="/resources/gfx/jiraf.png" type="image/x-icon">
    </head>
    
    <body>
        <div id="index-container">
            <div id="central-block">
                <header>
                    <span>Ooops! some error occurred</span>
                </header>
            
                <p>Failed to refer to <b>${fn:escapeXml (uri)}</b></p>
                
                <c:if test="${code != null && reason != null}">
                    <p><i><b>${code}</b> - ${reason}</i></p>
                </c:if>
            </div>
        </div>
    </body>
</html>