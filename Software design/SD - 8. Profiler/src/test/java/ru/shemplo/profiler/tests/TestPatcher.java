package ru.shemplo.profiler.tests;

import static org.junit.jupiter.api.Assertions.*;
import static org.objectweb.asm.Opcodes.*;

import java.lang.reflect.Array;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

import ru.shemplo.profiler.ExtendedClassLoader;
import ru.shemplo.profiler.asm.BeforeAfterPatcher;
import ru.shemplo.profiler.asm.ClassPatcher;
import ru.shemplo.profiler.log.Event;
import ru.shemplo.profiler.log.EventLogger;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowflakeInitializer;

public class TestPatcher {
    
    private static final String CLASS_NAME = "TestDummy";
    
    public static final EventLogger LOGGER = new EventLogger () {
        private int counter = 0;
        
        @Override
        public void onMethodStarted (String className, 
                String methodName, Object [] args) {
            counter += 1;
        }
        @Override
        public void onMethodFinished (String className, 
                String methodName) {
            counter += 1;
        }
        @Override
        public List <Event> getEvents () {
            return Arrays.asList ((Event []) Array.newInstance (Event.class, counter));
        }
    };
    
    private ExtendedClassLoader classLoader = new ExtendedClassLoader ();
    private ClassPatcher patcher = new BeforeAfterPatcher ();
    private byte [] dummy;
    
    @BeforeEach public void beforeAll () {
        SnowflakeInitializer <EventLogger> initializer = new SnowflakeInitializer <> (LOGGER, 1);
        Snowball.getContext ().registeredSnowflakes.put (EventLogger.class, initializer);
        initializer.rememberContext (new Object [0]);
        
        ClassWriter writer = new ClassWriter (0);
        writer.visit              (V1_8, ACC_PUBLIC + ACC_SUPER, CLASS_NAME, 
                                   null, "java/lang/Object", null);
        writer.visitSource        (CLASS_NAME.concat (".java"), null);
                                 
        MethodVisitor method = writer.visitMethod 
                                  (ACC_PUBLIC, "<init>", "()V", null, null);
        method.visitCode          ();
        Label l0 = new Label      ();
        method.visitLabel         (l0);
        method.visitVarInsn       (ALOAD, 0);
        method.visitMethodInsn    (INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        method.visitInsn          (RETURN);
        Label l1 = new Label      ();
        method.visitLabel         (l1);
        method.visitLocalVariable ("this", "LTestDummy;", null, l0, l1, 0);
        method.visitMaxs          (1, 1);
        method.visitEnd           ();
        
        method = writer.visitMethod (ACC_PUBLIC + ACC_STATIC + ACC_VARARGS, "main", 
                                     "([Ljava/lang/String;)V", null, null);
        method.visitCode          ();
        Label l2 = new Label      ();
        method.visitLabel         (l2);
        method.visitFieldInsn     (GETSTATIC, "java/lang/System", "out", 
                                   "Ljava/io/PrintStream;");
        method.visitLdcInsn       ("Hello world!");
        method.visitMethodInsn    (INVOKEVIRTUAL, "java/io/PrintStream", "println", 
                                   "(Ljava/lang/String;)V", false);
        method.visitInsn          (RETURN);
        Label l3 = new Label      ();
        method.visitLabel         (l3);
        method.visitLocalVariable ("args", "[Ljava/lang/String;", null, l2, l3, 0);
        method.visitMaxs          (3, 1);
        method.visitEnd           ();
        
        writer.visitEnd ();
        dummy = writer.toByteArray ();
    }
    
    @Test
    public void testPatcher () {
        byte [] bytes = patcher.patch (dummy);
        
        try {
            Class <?> type = classLoader.defineClass (CLASS_NAME, bytes);
            type.getDeclaredMethod ("main", String [].class)
                .invoke (null, (Object) new String [0]);
        } catch (Exception e) { e.printStackTrace (); }
        
        assertEquals (2, LOGGER.getEvents ().size ());
    }
    
}
