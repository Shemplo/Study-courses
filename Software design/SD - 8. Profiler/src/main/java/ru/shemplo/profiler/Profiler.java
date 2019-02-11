package ru.shemplo.profiler;

import java.io.*;
import java.lang.reflect.InvocationTargetException;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Stack;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Collectors;

import java.util.concurrent.atomic.AtomicInteger;

import ru.shemplo.profiler.asm.ClassPatcher;
import ru.shemplo.profiler.log.Event;
import ru.shemplo.profiler.log.Event.EventType;
import ru.shemplo.profiler.log.EventLogger;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.annot.Wind;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.utils.ClasspathUtils;

@Wind (blow = {ClassPatcher.class, EventLogger.class})
public class Profiler extends Snowball {
    
    public static void main (String ... args) throws Exception { shape (args); }
    
    public static String toName (String path) {
        return path.replace ('/', '.');
    }
    
    public static String toPath (String className) {
        return className.replace ('.', '/');
    }
    
    
    
    private ExtendedClassLoader classLoader;
    private ClassPatcher classPatcher;
    private EventLogger eventLogger;
    
    @Snowflake (manual = true)
    private String mainClass;
    
    @Override
    protected void onShaped (String ... args) {
        try {
            JarFile jar = new JarFile ("profiler.test.jar");
            profileJarFile (jar);
        } catch (IOException ioe) {
            
        }
    }
    
    public void profileJarFile (JarFile jar) {
        mainClass = getMainClass (jar);
        
        System.out.println (">> Reading JAR file <<");
        List <JarEntry> classFiles = ClasspathUtils.getEntriesFromJAR (jar).stream ()
                                   . filter  (e ->  e.getName ().endsWith (".class"))
                                   . filter  (e -> !e.getName ().contains ("package-info"))
                                   . collect (Collectors.toList ());
        classFiles.forEach (f -> {
            byte [] content = classPatcher.patch (readAllBytes (jar, f));
            String className = f.getName ().replace (".class", "")
                             . replace ('/', '.');
            
            File file = new File ("generated", toName (f.getName ()));
            try {
                if (!file.getParentFile ().exists ()) {
                    file.getParentFile ().mkdir ();
                }
                if (!file.exists ()) { file.createNewFile (); }                
            } catch (IOException ioe) { ioe.printStackTrace (); }
            
            try (OutputStream os = new FileOutputStream (file)) {
                os.write (content);
            } catch (IOException ioe) { ioe.printStackTrace (); }
            
            classLoader.defineClass (className, content);
        });
        
        try {
            System.out.println (">> Running main class of JAR file <<");
            Class <?> type = classLoader.loadClass (mainClass);
            type.getDeclaredMethod ("main", String [].class)
                .invoke (null, (Object) new String [] {});
        } catch (ClassNotFoundException cnfe) {
            cnfe.printStackTrace ();
            System.exit (1);
        } catch (IllegalAccessException | IllegalArgumentException 
              | NoSuchMethodException | InvocationTargetException 
              | SecurityException es) {
            es.printStackTrace ();
            System.exit (1);
        }
        
        System.out.println (">> Computing statistics <<");
        final List  <Event>   trace = new ArrayList <> (); 
        final Stack <Integer> stack = new Stack <> ();
        AtomicInteger counter = new AtomicInteger ();
        eventLogger.getEvents ().stream ()
                   .sorted  ((a, b) -> Double.compare (a.getTime (), b.getTime ()))
                   .forEach (event -> {
                       if (event.getEventType ().equals (EventType.START)) {
                           stack.push (counter.getAndIncrement ());
                           trace.add  (event);
                       } else {
                           final Integer index = stack.pop ();
                           Event start = trace.get (index);
                           
                           assert start.getMethodName ().equals (event.getMethodName ());
                           assert start.getClassName ().equals (event.getClassName ());
                           double time = (double) event.getTime () - start.getTime ();
                           start.setTime (time / 1_000_000); // to ms
                           start.setDepth (stack.size ());
                       }
                   });
        trace.forEach (event -> {
            String offset = new String (new char [event.getDepth () * 4]).replace ('\0', ' ');
            System.out.println (String.format (Locale.ENGLISH, "%s> %s # %s - %.2f [ms]", offset, 
                                               event.getClassName (), event.getMethodName (), 
                                               event.getTime ()));
        });
    }
    
    private String getMainClass (JarFile jar) {
        String mainClass = ClasspathUtils.getMainClassFromJar (jar)
                         . orElse (null);

        if (mainClass == null) {
           final String message = "Main class not found";
           throw new IllegalStateException (message);
        }
        
        return mainClass;
    }
    
    private byte [] readAllBytes (JarFile jar, JarEntry entry) {
        try (
            ByteArrayOutputStream baos = new ByteArrayOutputStream ();
            InputStream           is   = jar.getInputStream (entry);
        ) {
            byte [] buffer = new byte [1024 * 4];
            int read = -1;
            
            while ((read = is.read (buffer)) != -1) {
                baos.write (buffer, 0, read);
            }
            
            return baos.toByteArray ();
        } catch (IOException ioe) {
            throw new IllegalStateException (ioe);
        }
    }
    
}
