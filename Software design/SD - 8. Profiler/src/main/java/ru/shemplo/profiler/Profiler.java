package ru.shemplo.profiler;

import java.io.*;

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
            OutputStream os = new FileOutputStream ("Main.class");
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
            
            os.write (cw.toByteArray ());
            
            token.getDeclaredMethod ("main", String [].class).invoke (null, (Object) new String [0]);
            token.getDeclaredMethod ("methodA", String.class).invoke (null, "test");
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
            return new MyMethodVisitor (superMV);
        }
        
    }
    
    private static class MyMethodVisitor extends MethodVisitor {
        
        public MyMethodVisitor (MethodVisitor visitor) {
            super (Opcodes.ASM5, visitor);
            
            //Label l0 = new Label ();
            //super.visitLabel (l0);
            String namePS = PrintStream.class.getName ().replace ('.', '/'),
                   nameS  = System.class.getName ().replace ('.', '/');
            super.visitFieldInsn  (Opcodes.GETSTATIC, nameS, "out", "L" + namePS + ";");
            super.visitIntInsn    (Opcodes.BIPUSH, 2); // It's just a byte but not integer :(
            super.visitMethodInsn (Opcodes.INVOKEVIRTUAL, namePS, "println", "(I)V", false);
        }
        
        @Override
        public void visitInsn (int opcode) {
            if (opcode == Opcodes.RETURN) {
                //Label l1 = new Label ();
                //super.visitLabel (l1);
                String namePS = PrintStream.class.getName ().replace ('.', '/'),
                       nameS  = System.class.getName ().replace ('.', '/');
                super.visitFieldInsn  (Opcodes.GETSTATIC, nameS, "out", "L" + namePS + ";");
                super.visitIntInsn    (Opcodes.BIPUSH, 3); // It's just a byte but not integer :(
                super.visitMethodInsn (Opcodes.INVOKEVIRTUAL, namePS, "println", "(I)V", false);
            }
            
            super.visitInsn (opcode);
        }
        
    }
    
    private static class MyClassLoader extends ClassLoader {
        
        public Class <?> defineClass (String name, byte [] bytes) {
            return defineClass (name, bytes, 0, bytes.length);
        }
        
    }
    
}
