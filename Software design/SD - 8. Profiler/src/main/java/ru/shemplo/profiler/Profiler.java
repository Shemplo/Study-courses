package ru.shemplo.profiler;

import java.io.*;
import java.lang.reflect.InvocationTargetException;

import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Collectors;

import ru.shemplo.profiler.asm.ClassPatcher;
import ru.shemplo.profiler.log.EventLogger;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.annot.Wind;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.utils.ClasspathUtils;

@Wind (blow = {ClassPatcher.class, EventLogger.class})
public class Profiler extends Snowball {
    
    public static void main (String ... args) throws Exception { shape (args); }
    
    private ExtendedClassLoader classLoader;
    private ClassPatcher classPatcher;
    
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
        
        List <JarEntry> classFiles = ClasspathUtils.getEntriesFromJAR (jar).stream ()
                                   . filter  (e ->  e.getName ().endsWith (".class"))
                                   . filter  (e -> !e.getName ().contains ("package-info"))
                                   . collect (Collectors.toList ());
        classFiles.forEach (f -> {
            byte [] content = classPatcher.patch (readAllBytes (jar, f));
            String className = f.getName ().replace (".class", "")
                             . replace ('/', '.');
            classLoader.defineClass (className, content);
        });
        
        try {
            Class <?> type = classLoader.loadClass (mainClass);
            type.getDeclaredMethod ("main", String [].class)
                .invoke (null, (Object) new String [] {});
        } catch (ClassNotFoundException cnfe) {
            cnfe.printStackTrace ();
        } catch (IllegalAccessException | IllegalArgumentException 
              | NoSuchMethodException | InvocationTargetException 
              | SecurityException es) {
            es.printStackTrace();
        }
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
