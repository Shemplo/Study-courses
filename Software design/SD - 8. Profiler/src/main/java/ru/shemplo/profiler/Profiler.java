package ru.shemplo.profiler;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.objectweb.asm.*;

import ru.shemplo.snowball.utils.ClasspathUtils;

public class Profiler {
    
    public static void main (String ... args) throws Exception {
        JarFile file = new JarFile ("profiler.test.jar");
        JarEntry mainClass = ClasspathUtils.getEntriesFromJAR (file).stream ()
                           . filter (e -> e.getName ().endsWith ("Main.class"))
                           . findFirst ().orElse (null);
        try (
            ByteArrayOutputStream baos = new ByteArrayOutputStream ();
            InputStream is = file.getInputStream (mainClass);
        ) {
            byte [] buffer = new byte [4096];
            int read = 0;
            
            while ((read = is.read (buffer, 0, buffer.length)) != -1) {
                baos.write (buffer, 0, read);
            }
            
            byte [] bytes = baos.toByteArray ();
            ClassReader cr = new ClassReader (bytes);
            ClassWriter cw = new ClassWriter (cr, ClassWriter.COMPUTE_FRAMES);
            
            ClassVisitor cv = new MyClassVisitor (cw);
            cr.accept (cv, ClassReader.EXPAND_FRAMES);
             
            MyClassLoader classLoader = new MyClassLoader ();
            Class <?> token = classLoader.defineClass ("ru.shemplo.profiler.Main", cw.toByteArray ());
            
            System.out.println (token.getDeclaredMethod ("main", String [].class)
                                     .invoke (Profiler.class, (Object) new String [0]));
            //token.getMethod ("main", String [].class).invoke (null, new Object [] {});
        }
        
    }
    
    private static class MyClassVisitor extends ClassVisitor {
        
        public MyClassVisitor (ClassVisitor visitor) {
            super (Opcodes.ASM5, visitor);
        }
        
        @Override
        public MethodVisitor visitMethod (int access, String name, String desc, 
                                          String signature, String [] exceptions) {
            MethodVisitor superMV = super.visitMethod (access, name, desc, 
                                                   signature, exceptions);
            /*
            MethodVisitor extended = super.visitMethod (access, name, desc, 
                                                    signature, exceptions);*/
            //extended = new MyMethodVisitor (superMV, extended);
            return new MyMethodVisitor (superMV);
        }
        
    }
    
    private static class MyMethodVisitor extends MethodVisitor {
        
        public MyMethodVisitor (MethodVisitor visitor) {
            super (Opcodes.ASM5, visitor);
            
            Label l0 = new Label ();
            super.visitLabel (l0);
            String namePS = PrintStream.class.getName ().replace ('.', '/'),
                   nameS  = System.class.getName ().replace ('.', '/');
            super.visitFieldInsn  (Opcodes.GETSTATIC, nameS, "out", "L" + namePS + ";");
            super.visitIntInsn    (Opcodes.BIPUSH, 1); // It's just a byte but not integer :(
            super.visitMethodInsn (Opcodes.INVOKEVIRTUAL, namePS, "println", "(I)V", false);
        }
        
    }
    
    private static class MyClassLoader extends ClassLoader {
        
        public Class <?> defineClass (String name, byte [] bytes) {
            return defineClass (name, bytes, 0, bytes.length);
        }
        
    }
    
}
