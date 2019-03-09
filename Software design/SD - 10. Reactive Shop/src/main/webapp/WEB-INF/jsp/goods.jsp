<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib uri = "http://java.sun.com/jsp/jstl/functions" prefix = "fn" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<!DOCTYPE HTML>
<html>
    <head>
        <meta charset="UTF-8" />
        <title>Sea of flowers :: goods</title>
        
        <link rel="stylesheet" href="/resources/css/goods.css" />
        <!--<link rel="shortcut icon" href="/resources/gfx/jiraf.png" type="image/x-icon">-->
    </head>
    
    <body>
        <c:set var="currency" value="${user.getUser ().getCurrency ().getCodeISO ()}" />
        <c:set var="withDescription" value="${user.getUser ().isWithDescription ()}" />
        <c:set var="identifier" value="${user.getUser ().getIdentifier ()}" />
        <c:set var="withIcon" value="${user.getUser ().isWithIcon ()}" />
        <c:set var="sorting" value="${user.getUser ().getSorting ()}" />
        <c:set var="color" value="${user.getUser ().getColor ()}" />
        <c:set var="shape" value="${user.getUser ().getShape ()}" />
        <c:set var="login" value="${user.getUser ().getLogin ()}" />
    
        <header class="top-page-header">
            <div class="top-page-header-background"></div>
            <div class="top-page-header-cover">
                <img alt="logo" src="/resources/gfx/sea.png" />
                <span>Sea of flowers</span>
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
                        <input id="userIdentifier" type="hidden" value="${identifier}" />
                        <p>Welcome, <b>${login}</b></p>
                        <br />
                        
                        <p><b>Choose what you want:</b></p>
                        
                        <p class="form-line">
                           <span>Sorting:</span>
                           <select id="sorting">
                               <option value="alphabetASC"  ${sorting == 'alphabetASC' ? 'selected' : ''}>A → Z</option>
                               <option value="alphabetDESC" ${sorting == 'alphabetDESC' ? 'selected' : ''}>Z → A</option>
                               <option value="priceDESC"    ${sorting == 'priceDESC' ? 'selected' : ''}>Price ↓</option>
                               <option value="priceASC"     ${sorting == 'priceASC' ? 'selected' : ''}>Price ↑</option>
                               <option value="shuffle"      ${sorting == 'shuffle' ? 'selected' : ''}>Shuffle</option>
                           </select>
                        </p>
                        
                        <p class="form-line">
                            <span>Currency:</span>
                            <select id="currency">
                                <c:forEach var="cur" items="${currencies}">
                                    <option ${cur == currency ? 'selected' : ''}>${cur}</option>
                                </c:forEach>
                            </select>
                        </p>
                        
                        <p class="form-line">
                            <span>Flowers with description:</span>
                            <input id="showWithoutDesc" type="checkbox" ${withDescription ? 'checked' : ''} />
                        </p>
                        <p class="form-line">
                            <span>Flowers with icon:</span>
                            <input id="showWithoutIcon" type="checkbox" ${withIcon ? 'checked' : ''} />
                        </p>
                        
                        <p class="form-line">
                           <span>Living shape:</span>
                           <select id="shape" disabled>
                                <c:forEach var="cur" items="${currencies}">
                                    <option ${cur == shape ? 'selected' : ''}>flower</option>
                                </c:forEach>
                           </select>
                        </p>
                        
                        <p class="form-line">
                           <span>Main color:</span>
                           <select id="color" disabled>
                                <option ${color == 'all' ? 'selected' : ''}>All</option>
                                <c:forEach var="cur" items="${currencies}">
                                    <option ${cur == color ? 'selected' : ''}>yellow</option>
                                </c:forEach>
                           </select>
                        </p>
                        
                        <p class="form-line">
                            <button id="filter">Apply parameters</button>
                        </p>
                    </c:otherwise>
                </c:choose>
            </div>
            
            <div class="goods-block">
                <c:forEach var="item" items="${items}">
                    <div class="good-item-line">
                        <div class="good-item-price">
                            <img alt="stub" src="/resources/${item.getThumbnail ()}">
                            ${item.getPrice ()} ${currency}
                        </div>
                        <div>
                            <h3>${item.getName ()}</h3>
                            <c:if test="${item.getDescription () != null}">
                                <p>${item.getDescription ()}</p>
                            </c:if>
                        </div>
                    </div>
                </c:forEach>
            </div>
        </div>
        
        <script type="text/javascript" src="/resources/js/goods.js"></script>
    </body>
</html>