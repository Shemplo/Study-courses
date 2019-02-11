package ru.shemplo.profiler.asm;

import static org.objectweb.asm.Opcodes.*;
import static ru.shemplo.snowball.stuctures.Pair.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.objectweb.asm.*;

import ru.shemplo.profiler.log.EventLogger;
import ru.shemplo.snowball.annot.Snowflake;
import ru.shemplo.snowball.annot.processor.Snowball;
import ru.shemplo.snowball.annot.processor.SnowballContext;
import ru.shemplo.snowball.stuctures.Pair;

@Snowflake (priority = 0)
public class BeforeAfterPatcher implements ClassPatcher {
    
    private static final String SCLASS_NAME = EventLogger.class.getName ()
                                            . replace ('.', '/'),
                                VAR_DESC    = String.format ("L%s;", SCLASS_NAME),
                                VAR_NAME    = "__eventLogger";
    
    @Override
    public byte [] patch (byte [] bytes) {
        ClassReader cr = new ClassReader (bytes);
        ClassWriter cw = new ClassWriter (cr, ClassWriter.COMPUTE_FRAMES);
        
        ClassVisitor cv = new BAClassVisitor (cw, cr.getClassName ());
        cr.accept (cv, ClassReader.EXPAND_FRAMES);
        return cw.toByteArray ();
    }
    
    private class BAClassVisitor extends ClassVisitor {
        
        private final String CLASS_NAME;
        
        public BAClassVisitor (ClassVisitor classVisitor, String className) {
            super (ASM7, classVisitor);
            CLASS_NAME = className;
            
            super.visitField (ACC_PRIVATE + ACC_STATIC + ACC_FINAL, VAR_NAME, 
                              VAR_DESC, null, null).visitEnd ();
            String snowballCName = SnowballContext.class.getName ().replace ('.', '/'), 
                   snowballName  = Snowball.class.getName ().replace ('.', '/');
            
            MethodVisitor mv = visitMethod (ACC_STATIC, "<clinit>", 
                                            "()V", null, null);
            mv.visitCode ();
            
            Label l0 = new Label ();
            mv.visitLabel      (l0);
            mv.visitMethodInsn (INVOKESTATIC, snowballName, "getContext", 
                                "()L" + snowballCName + ";", false);
            mv.visitLdcInsn    (Type.getType ("L" + SCLASS_NAME + ";"));
            mv.visitMethodInsn (INVOKEVIRTUAL, snowballCName, "getSnowflakeFor", 
                                "(Ljava/lang/Class;)Ljava/lang/Object;", false);
            mv.visitTypeInsn   (CHECKCAST, SCLASS_NAME);
            mv.visitFieldInsn  (PUTSTATIC, className, VAR_NAME, VAR_DESC);
            mv.visitInsn       (RETURN);
            mv.visitMaxs       (2, 0);
            mv.visitEnd        ();
        }
        
        @Override
        public MethodVisitor visitMethod (int access, String name, String descriptor, 
                                          String signature, String [] exceptions) {
            MethodVisitor mv = super.visitMethod (access, name, descriptor, signature, exceptions);
            boolean isStatic = (access & ACC_STATIC) == ACC_STATIC;
            
            return new BAMethodVisitor (mv, CLASS_NAME, name, typeOfArguments (descriptor), 
                                        isStatic);
        }
        
        private List <Pair <Class <?>, String>> typeOfArguments (String descriptor) {
            List <Pair <Class <?>, String>> arguments = new ArrayList <> ();
            char [] chars = descriptor.toCharArray ();
            int pointer = 1; // skip (
            
            while (pointer < chars.length && chars [pointer] != ')') {
                switch (chars [pointer]) {
                    case 'B': arguments.add (mp (Byte.class,      "B")); break;
                    case 'C': arguments.add (mp (Character.class, "C")); break;
                    case 'D': arguments.add (mp (Double.class,    "D")); break;
                    case 'F': arguments.add (mp (Float.class,     "F")); break;
                    case 'I': arguments.add (mp (Integer.class,   "I")); break;
                    case 'J': arguments.add (mp (Long.class,      "J")); break;
                    case 'S': arguments.add (mp (Short.class,     "S")); break;
                    case 'Z': arguments.add (mp (Boolean.class,   "Z")); break;
                    case 'L': case '[': {
                        while (pointer + 1 < chars.length && chars [++ pointer] != ';');
                        arguments.add (null); // don't need convert
                    } break;
                    
                    default:
                        throw new IllegalStateException ("Unknown char " + chars [pointer] 
                                                         + " in method descriptor");
                }
                
                pointer += 1; // next character
            }
            
            return arguments;
        }
        
    }
    
    private class BAMethodVisitor extends MethodVisitor {

        private final Integer []    RETURN_CODES_A = {RETURN, IRETURN, LRETURN, FRETURN, 
                                                      DRETURN, ARETURN};
        private final Set <Integer> RETURN_CODES   = Arrays.asList (RETURN_CODES_A).stream ()
                                                   . collect (Collectors.toSet ());
        
        private final String CLASS_NAME, METHOD_NAME;
        
        public BAMethodVisitor (MethodVisitor methodVisitor, String className, String methodName, 
                                List <Pair <Class <?>, String>> args, boolean isStatic) {
            super (ASM7, methodVisitor);
            METHOD_NAME = methodName;
            CLASS_NAME  = className;
            
            if (!METHOD_NAME.equals ("<clinit>") && !METHOD_NAME.equals ("<init>")) {
                //  __eventLogger.onMethodStarted (*CLASS_NAME*, *METHOD_NAME*, *ARGUMENTS*);
                Label l0 = new Label ();
                visitLabel (l0);
                visitFieldInsn (GETSTATIC, CLASS_NAME, VAR_NAME, VAR_DESC);
                visitLdcInsn   (CLASS_NAME);
                visitLdcInsn   (METHOD_NAME);
                visitIntInsn   (BIPUSH, args.size ());
                visitTypeInsn  (ANEWARRAY, "java/lang/Object");
                
                int shift = isStatic ? 0 : 1; // in non-static there is `this`
                for (int i = 0; i < args.size (); i++) {
                    visitInsn    (DUP);
                    visitIntInsn (BIPUSH, i);
                    if (args.get (i) != null && args.get (i).F != null) {
                        String letter = args.get (i).S;
                        int opcode = letter.equals ("D") ? DLOAD
                                   : letter.equals ("F") ? FLOAD
                                   : letter.equals ("J") ? LLOAD
                                   : ILOAD;
                        visitIntInsn (opcode, i + shift);
                        if (letter.equals ("D") || letter.equals ("J")) {
                            // long & double takes 2 positions on stack
                            shift += 1; 
                        }
                        String owner = args.get (i).F.getName ().replace ('.', '/'),
                               desc  = String.format ("(%s)L%s;", letter, owner);
                        visitMethodInsn (INVOKESTATIC, owner, "valueOf", desc, false);
                    } else {
                        // Here can be only references
                        visitVarInsn (ALOAD, i + shift);
                    }
                    visitInsn (AASTORE);
                }
                
                String desc =  "(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)V";
                visitMethodInsn (INVOKEINTERFACE, SCLASS_NAME, "onMethodStarted", desc, true);
            }
        }
        
        @Override
        public void visitInsn (int opcode) {
            if (!METHOD_NAME.equals ("<clinit>") && !METHOD_NAME.equals ("<init>")
                    && RETURN_CODES.contains (opcode)) {
                //  __eventLogger.onMethodFinished (*CLASS_NAME*, *METHOD_NAME*);
                final String desc = "(Ljava/lang/String;Ljava/lang/String;)V";
                
                Label l0 = new Label ();
                visitLabel      (l0);
                visitFieldInsn  (GETSTATIC, CLASS_NAME, VAR_NAME, VAR_DESC);
                visitLdcInsn    (CLASS_NAME);
                visitLdcInsn    (METHOD_NAME);
                visitMethodInsn (INVOKEINTERFACE, SCLASS_NAME, "onMethodFinished", desc, true);
            }
            
            super.visitInsn (opcode);
        }
        
        @Override
        public void visitMaxs (int maxStack, int maxLocals) {
            super.visitMaxs (maxStack + 4, maxLocals);
        }
        
    }
    
}
