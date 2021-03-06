package ru.shemplo.profiler;

import java.io.*;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;

import java.util.*;
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
import ru.shemplo.snowball.stuctures.Pair;
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
        assert args.length > 0;
        
        final String fileName = args [0];
        args = subarray (args, 1);
        
        try {
            File file = new File (fileName);
            if (!file.canRead ()) {
                String message = "Failed to read JAR file";
                throw new IllegalStateException (message);
            }
            
            JarFile jar = new JarFile (file);
            profileJarFile (jar, args);
        } catch (IOException ioe) {
            
        }
    }
    
    @SuppressWarnings ("unchecked")
    private <T> T [] subarray (T [] array, int from) {
        if (from >= array.length && array.length > 0) {
            final Class <?> token = array [0].getClass ();
            return (T []) Array.newInstance (token, 0);
        }
        
        if (from < array.length && array.length > 0) {
            final Class <?> token = array [0].getClass ();
            T [] tmp = (T []) Array.newInstance (token, array.length - from);
            System.arraycopy (array, from, tmp, 0, tmp.length);
            return tmp;
        }
        
        return null;
    }
    
    public void profileJarFile (JarFile jar, String [] args) {
        mainClass = getMainClass (jar);
        
        System.out.println (">> Reading JAR file <<");
        List <JarEntry> classFiles = ClasspathUtils.getEntriesFromJAR (jar).stream ()
                                   . filter  (e ->  e.getName ().endsWith (".class"))
                                   . filter  (e -> !e.getName ().contains ("package-info"))
                                   . collect (Collectors.toList ());
        Queue <Pair <String, byte []>> classes = new LinkedList <> ();
        classFiles.forEach (f -> {
            byte [] content  = classPatcher.patch (readAllBytes (jar, f));
            String className = toName (f.getName ().replace (".class", ""));
            System.out.println (className);
            
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
            
            classes.add (Pair.mp (className, content));
        });
        
        while (!classes.isEmpty ()) {
            Pair <String, byte []> pair = classes.poll ();
            try {
                classLoader.defineClass (pair.F, pair.S);
            } catch (NoClassDefFoundError ncdfe) {
                classes.add (pair);
            }
        }
        
        try {
            System.out.println (">> Running main class of JAR file <<");
            Class <?> type = classLoader.loadClass (mainClass);
            type.getDeclaredMethod ("main", String [].class)
                .invoke (null, (Object) args);
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
