## JiraF - Онлан список задач
> Простой способ вспомнить о своих делах...

Протестировать можно тут: [shemplo.ru:8081](http://shemplo.ru:8081)

### Структура прокта

Проект выполнен с использованием фреймворка _[Spring](http://spring.io)_ 
и его модуля _[Spring Boot](https://spring.io/projects/spring-boot)_

Концепция этого модуля подразумевает использование паттерна проектирования **MVC** 
(**M**odel **V**iew **C**ontroller), 
поэтому и структура проекта организована по тому же принципу.

```java
  Структура проеста в виде дерева директорий:
  
  Пакеты
  ru.shemplo.tasks             // Основной пакет приложения. Содержит класс запуска и загрузки конфигурации.
                 |- conf       // Пакет, содержащий классы конфигурирования Spring Boot приложения
                 |- db         // Пакет, содержащий классы для работы с базой данных
                 |- mvc        // Промежуточный пакет, для явного выделения основных частей MVC
                      |- cont  // Пакет, содержащий классы для выполнения бизнес-логики 
                      |        // и управления запросами к серверу
                      |- model // Пакет, содержащий классы, описывающие модель визуальной части
                       
  Ресурсы
  resources                         // Директория, хранящая все ресурсы (за исключением JSP шаблонов страниц)
          |- static                 // Директория с неизменяемыми ресурсами
                  |- css            // Файлы стилей
                  |- gfx            // Изображения и иконки
                  |- js             // Скрипты JavaScript для клиентской части
          |- template               // Специальная директория для изменяемых ресурсов
          |                         // (Может быть использована сторонними фреймворками типа Thymeleaf, ...)
          |                         // (Сейчас не используется)
          |* application.properties //
          
  Шаблоны страниц
  webapp               // Директория, необходимая для веб-приложений 
       |- WEB-INF      // Директория, с файлами конфигурирования веб-приложения 
                |      // (без использования Spring Boot)
                |- jsp // Шаблоны веб-страниц в JSP формате с поддержкой JSTL шаблонизатора
```

### Модель проекта

По сути, модель состоит из 2<sup>х</sup> сущностей:
* **Задание**  
  Сущность задания содежит в себе информацию о том, что надо сделать, когда и сделано ли это уже.  
  Поный код: [Task.java](src/main/java/ru/shemplo/tasks/mvc/model/Task.java)
```java
  class Task {
      String     description; // Содержание задания (описание)
      TaskStatus status;      // IN_PROCESS | DONE | FAILED
      Date       expire;      // Время, до которого надо сделать
      long       ID;          // Идентификатор в базе данных
  }
```
* **Список заданий**  
  Сущность списка содержит в себе название этого списка и задания, которые ему приписаны.  
  Поный код: [ListOfTasks.java](src/main/java/ru/shemplo/tasks/mvc/model/ListOfTasks.java)
```java
  class ListOfTasks {
      String title;      // Заголовок списка (название)
      long   ID;         // Идентификатор в базе данных
      
      List <Task> tasks; // Список заданий
  }
```

Классы этих сущностей реализованы в точности по модели 
и содержат дополнительные методы для построения объекта 
из результата запроса к базе данных. Пример для _ListOfTasks_:
```java
  public static ListOfTasks valueFrom (ResultSet result) throws SQLException {
      String title = result.getString ("title");
      long id = result.getLong ("id");
        
      return new ListOfTasks (id, title);
  }
```

### Управление запросами к серверу

_Spring Boot_ позволяет управлять запросами с помощью Java-аннотаций у методов и классов. 
Для того, чтобы класс считался контролером, его достаточно отметить аннотацией `@Controller`.
После этого к етодам можно навешивать аннотации типа `@RequestMapping`, `@GetMapping`, `@PostMappring`, ..., 
чтобы назначить метод отвественным за обрботку зпроса на заданный URI.

Полный код контроллера: [RequestController.java](src/main/java/ru/shemplo/tasks/mvc/cont/RequestController.java)

Пример метода, обрабатывающий запрос на индексную (главную) страницу сервиса:
```java
  @GetMapping (path = "/")
  public ModelAndView handleIndexPage () {
      // handle process
  }
```

Для обработки запросов к странице со списком заданий используется метод:
```java
  @GetMapping (path = "/lists")
  public ModelAndView handleTaskLists () {
      // handle process
  }
```

Для обработки запросов на изменения - метод:
```java
  @PostMapping (path = "/lists/{operation}/{aim}")
  @ResponseBody
  public String handleAPIRequestAdd (@PathVariable ("operation") String operation, 
          @PathVariable ("aim") String aim, @RequestBody String body) {
      // handle process
  }
```

У этого метода дополнительно используются аннотации 
`@ResponseBody` - результат должен быть напрямую включён в ответ от сервера, без дальнейшеей обработки; 
`@PathVariable` - получение значения из шаблона пути запроса; 
`@RequestBody` - тело запроса от клиента;

Ошибки, которые возникают из-за того, чо какой-то ресурс не найден, обрабатываются методом:
```java
  @ResponseStatus (HttpStatus.NOT_FOUND)
  @ExceptionHandler (NoHandlerFoundException.class)
  public ModelAndView handleNotFound (HttpServletRequest req, 
          HttpServletResponse resp, Exception ex) {
      // handle process
  }
```

Для того, чтобы этот метод срабатывал, на класс ещё необходимо навесить аннотацию `@ControllerAdvice`.

### Выполнение бизнес-логики

Непосредственно вся бизнес-логика находится в 2<sup>х</sup> классах:
* **RequestOperation**  
  Это `enum` с константами, которые содержат функции обраотки, вызываемые из метода `handleAPIRequestAdd ()`.
  Полный код: [RequestOperation.java](src/main/java/ru/shemplo/tasks/mvc/cont/RequestOperation.java)
```java
  DELETE_TASK ((db, r) -> { // функция удаление задания из базы данных
      // some actions  
  }, "task"); // список аргументов, которые должны быть в теле запроса
```
* **DBAccess**  
  Все операции с базой данных, необходимые для работы сервиса.  
  Полный код: [DBAccess.java](src/main/java/ru/shemplo/tasks/db/DBAccess.java)
  
  ### Визуальное отображение
  
  В качестве ответа на `GET` запрос возвращается сгенерированная _HTML_ страница.
  
  Для генерации используется шаблонизатор _[JSTL](https://mvnrepository.com/artifact/javax.servlet.jsp.jstl/jstl-api)_, 
  позволяющий с помощью _HTML_-тегов управлять генерацией.
  
  Полный код: [index.jsp](src/main/webapp/WEB-INF/jsp/index.jsp)
  
  Часть кода, которая генерирует блок с описанием задания:
  ```jsp
  <li task="${task.getID ()}"                                <!-- Стандартный элемент списка -->
      class="task ${styleClass}">
      <span>${fn:escapeXml (task.getDescription ())}</span>  <!-- Описание задания с экранированными символами  -->
      <c:if test="${task.hasExpireDate ()}">                 <!-- Проверка условия на наличие даты окончания -->
          <span><b>Till:</b> ${task.getExpireDate ()}</span> <!-- При наличии даты, вывести информацию о ней -->
      </c:if>
      <span><b>Status:</b> ${status}</span>                  <!-- Вывод текущего статуса задания -->
      <button class="delete-task-button">                    <!-- Кнопка удаления задания -->
          <img src="/resources/gfx/bin.png" />               <!-- Внутренность кнопки - иконка корзины -->
      </button>
  </li>
  ```
