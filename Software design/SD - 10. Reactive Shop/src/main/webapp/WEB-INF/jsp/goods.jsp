<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri = "http://java.sun.com/jsp/jstl/functions" prefix = "fn" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<!DOCTYPE HTML>
<html>
    <head>
        <meta charset="UTF-8" />
        <title>Shop :: goods</title>
        
        <link rel="stylesheet" href="/resources/css/goods.css" />
        <!--<link rel="shortcut icon" href="/resources/gfx/jiraf.png" type="image/x-icon">-->
    </head>
    
    <body>
        <c:set var="currency" value="${user.getUser ().getCurrency ().getCodeISO ()}" />
    
        <header class="top-page-header">
            <div class="top-page-header-background"></div>
            <div class="top-page-header-cover">
                <img alt="logo" src="/resources/gfx/sea.png" />
                <span>Sea of Flowers</span>
            </div>
        </header>
    
        <div class="two-columns-block">
            <div class="guest-block form">
                <c:choose>
                    <c:when test="${user.getUser ().getIdentifier () == 'guest'}">
	                    <p>You are identified as <b>guest</b></p>
	                    
	                    <p>
	                       If you want to have more flexible <br />
	                       options, you can assign yourself:
	                    </p>
	                    
	                    <p class="form-line">
	                       <span>Your login:</span>
	                       <input id="login" type="text" />
	                    </p>
	                    
	                    <p class="form-line">
	                       <span>Preffered currency:</span>
	                       <select id="currency">
	                           <c:forEach var="cur" items="${currencies}">
	                               <option>${cur}</option>
	                           </c:forEach>
	                       </select>
	                    </p>
	                    
	                    <p class="form-line">
	                       <button id="signup">Sign up</button>
	                    </p>
	                </c:when>
	                
	                <c:otherwise>
                        Not guest
                    </c:otherwise>
                </c:choose>
            </div>
            
            <div class="goods-block">
                <c:forEach var="itemObj" items="${items}">
                    <c:set var="item" value="${itemObj.getItem ()}" />
                    <p class="good-item-line">
                        <img alt="stub" src="/resources/${item.getThumbnail ()}">
                        <span class="good-item-price">${item.getPrice ()} ${currency}</span>
                        ${item.getName ()}
                    </p>
                </c:forEach>
            </div>
        </div>
        
        <script type="text/javascript" src="/resources/js/goods.js"></script>
    </body>
</html>