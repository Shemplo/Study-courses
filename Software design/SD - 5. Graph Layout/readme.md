## Graph layout - есть только один путь и провал
> О том, как управлять альтернативами в нереальном времени...

**Цель работы:** получить практический опыт применения структурного паттерна _bridge_.

Необходимо реализовать простой визуализатор графов, используя два различных графических API. 
Способ визуализации графа можно выбрать самостоятельно (например, рисовать вершины по кругу). 
Приложение должно поддерживать две реализации графов: на спискахребер и матрице смежностей.

**Примечания:**
* выбор API и реализации графа должны задаваться через аргументы командной строки при запуске приложения;
* можно использовать любой язык и любые API для рисования (главное, чтобы они были принципиально разные).

### Реализация

Зафиксируем параметры, для которых предполагаются альтернативы:  
Полностью код: [Parameter.java](src/main/java/ru/shemplo/graphlay/io/Parameter.java)
```java
  public enum Parameter {
      FORMAT      ("-f"),  // Формат входного файла с описанием графа
      DRAWING_API ("-da"), // Технология, которую надо использовать для рендеринга графа
      GRAPH_FILE  ("-gf"); // (Дополнительный параметр) Файл с описанием графа
  }
```
При этом добавим флаг, который отвечает за "обязательность" параметра, 
и ещё сделаем значения по умолчанию, которые будут возвращены, 
если значение параметра не найдено при разборе аргументов командной строки.

Далее для первых двух параметров заведём свои _enum_'ы, 
потому что они должны отвечать за инициализацию новых объектов, 
которые соотвествуют какому-то значению параметра.

Полностью код: [GraphFormat.java](src/main/java/ru/shemplo/graphlay/GraphFormat.java)
```java
  public enum GraphFormat {
      EDGES  (() -> new EdgesGraphReader  ()), // Описание графа через список рёбер
      MATRIX (() -> new MatrixGraphReader ()); // Описание графа через матрицу смежности
  }
```

Полностью код: [RenderType.java](src/main/java/ru/shemplo/graphlay/RenderType.java)
```java
  public enum RenderType {
      AWT    (() -> new AWTGraphRender    (800, 600)), // Использовать Java Swing (и java.lang.awt.Canvas) для отрисовки
      JAVAFX (() -> new JavaFXGraphRender (800, 600)); // Использовать JavaFX для отрисовки графа
  }
```
> _По-хорошему, для того,чтобы не разносить конфигурацию на разные файлы (и к тому же избавиться от copy&paste),
можно было сделать перечисление возможных значений внутри "главного" файла концигурации <u>Parameter.java</u>_

Сама структура графа содержится в классе 
[Graph.java](src/main/java/ru/shemplo/graphlay/graph/Graph.java)
и представляет собой список рёбер и множество вершин:
```java
  public class Graph {
      
      private List <Pair <I, I>> EDGES;
      private Set <I> VERTEXES;
      
      public void setOrientated (boolean orientated); // (Не реализовано в полной мере) 
                                                      // Ориентированный / неориентированный граф
      
      public void addVertex (int vertex);             // Добавление вершины в граф
      
      public void addEdge (int from, int to);         // Добавление ребра в граф
      
      public void render (GraphRender render);        // Отрисовка графа на канве
      
  }
```

Объект этого класса генерирутеся объектом типа `GraphReader`, который описывается интерфейсом:  
Полностью код: [GraphReader.java](src/main/java/ru/shemplo/graphlay/io/GraphReader.java)
```java
  public interface GraphReader {
  
      // Прочитать файл по заданному пути и создать из него объект типа Graph
      public Graph read (String path) throws IOException;
  
  }
```

Для отрисовки графа используется объект типа `GraphRender`с интерфейсом:  
Полностью код: [GraphRender.java](src/main/java/ru/shemplo/graphlay/gfx/GraphRender.java)
```java
  public interface GraphRender {
      
      // Провести отрезок, заданный двумя концами
      public void strokeLine (double fx, double fy, double tx, double ty);
    
      // Нарисовать окружность
      public void strokeCircle (double x, double y, double r);
      // Нарисова круг
      public void fillCircle (double x, double y, double r);
    
      // Установить цвет линии (границы)
      public void setStroke (Color color);
      // Установить цвет внутренности
      public void setFill (Color color);
    
      // Усановить ширину линии отрезка
      public void setLineWidth (double width);
    
  }
```
