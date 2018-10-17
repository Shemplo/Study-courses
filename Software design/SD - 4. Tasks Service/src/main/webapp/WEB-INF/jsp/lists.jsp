<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<!DOCTYPE HTML>
<html>
    <c:set var="listsPresent" value="${listsOfTasks != null && not empty listsOfTasks}" />
    
    <head>
        <meta charset="UTF-8" />
        <title>Tasks page</title>
        
        <link rel="stylesheet" href="/resources/css/lists.css" />
    </head>
    
    <body>
        <header id="main-header">
            <span>JiraF</span><img src="/resources/gfx/jiraf.png" />
        </header>
        
        <div id="control-container">
            <button id="add-list-button">
                <img src="/resources/gfx/add.png" /> <span>task list</span>
            </button>
        </div>

        <c:if test="${listsPresent}">
            <div id="lists-container">
                <c:forEach var="list" items="${listsOfTasks}">
                    <div>
                        <header>${list.getHeader ()}</header>
                        
                        <div class="list-control-container">
	                        <button list="${list.getID ()}" 
	                                title="${list.getHeader ()}" 
	                                class="add-task-button">
	                           <img src="/resources/gfx/add.png" />
	                        </button>
	                        <button>
	                           <img src="/resources/gfx/bin.png" />
	                        </button>
	                    </div>
	                    
	                    <c:set var="tasks" value="${list.getTasks ()}" />
	                    
	                    <c:choose>
	                       <c:when test="${tasks != null && not empty tasks}">
	                           <ul>
	                               <c:forEach var="task" items="${tasks}">
	                                   <c:set var="styleClass" value="${task.getStatus ().getStyle ()}" />
	                                   <c:set var="status" value="${task.getStatus ().toString ()}" />
                                       
	                                   <li class="${styleClass}">
	                                       <span>${task.getDescription ()}</span>
	                                       <c:if test="${task.hasExpireDate ()}">
	                                           <span><b>Till:</b> ${task.getExpireDate ()}</span>
	                                       </c:if>
	                                       <span><b>Status:</b> ${status}</span>
	                                       <button><img src="/resources/gfx/bin.png" /></button>
	                                   </li>
	                               </c:forEach>
	                           </ul>
	                       </c:when>
	                       
	                       <c:otherwise>
	                           <div class="no-tasks-message">Soon here will be something...</div>
	                       </c:otherwise>
	                    </c:choose>
                    </div>
                </c:forEach>
                <div>
                    <header>University tasks</header>
                    
                    <div class="list-control-container">
                        <button class="add-task-button"><img src="/resources/gfx/add.png" /></button>
                        <button><img src="/resources/gfx/bin.png" /></button>
                    </div>
                    
                    <ul>
                        <li>
                            <span>Do homework of SD till the saturday</span>
                            <span><b>Status:</b> in process</span>
                            
                            <button><img src="/resources/gfx/bin.png" /></button>
                        </li>
                        <li>
                            <span>Write good Diploma work</span>
                            <span><b>Status:</b> in process</span>
                            
                            <button><img src="/resources/gfx/bin.png" /></button>
                        </li>
                    </ul>
                </div>
            </div>
        </c:if>
        <c:if test="${!listsPresent}">
            <p id="no-lists-message">
                No task lists available... try to <b>add</b> new one
            </p>
        </c:if>
        
        <div id="add-list-shadow" class="shadow ma">
            <div>
                <header>Add new list</header>
                <div>Title: <input type="text" /></div>
                <div><span></span><button>add list</button></div>
            </div>
        </div>
        
        <div id="add-task-shadow" class="shadow">
            <div>
                <header>Add new task</header>
                <div>Description: <input type="text" /></div>
                <div>Expire date: <div><input type="date" /> <input type="time" /></div></div>
                <div>List: <div>University tasks</div></div>
                <div><span></span><button>add task</button></div>
            </div>
        </div>
        
        <script type="text/javascript" src="/resources/js/index.js"></script>
    </body>
</html>