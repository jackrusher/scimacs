package com.rusher.libscimacs;

import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.struct.CFieldAddress;
import org.graalvm.nativeimage.c.struct.CStruct;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.nativeimage.c.type.WordPointer;
import org.graalvm.word.PointerBase;
import com.oracle.svm.core.c.CConst;

import com.rusher.scimacs;

public final class LibScimacs {
    @CFunction("eval_in_emacs")
    public static native @CConst CCharPointer eval_in_emacs(@CConst WordPointer emacs_env,
                                                            @CConst CCharPointer func,
                                                            @CConst CCharPointer param);
    
    @CEntryPoint(name = "sci_eval_string")
    public static @CConst CCharPointer evalString(@CEntryPoint.IsolateThreadContext long isolateId,
                                                  @CConst WordPointer emacs_env,
                                                  @CConst CCharPointer clojure_string) {
        String expr = CTypeConversion.toJavaString(clojure_string);
        String result = com.rusher.scimacs.evalString((Object func, Object param) -> {
                CTypeConversion.CCharPointerHolder func_holder = CTypeConversion.toCString((String)func);
                CTypeConversion.CCharPointerHolder param_holder = CTypeConversion.toCString((String)param);
                CCharPointer func_value = func_holder.get();
                CCharPointer param_value = param_holder.get();
                CCharPointer c_str_result = eval_in_emacs(emacs_env, func_value, param_value);
                String str_result = CTypeConversion.toJavaString(c_str_result);
                // TODO MEMORY LEAK. Must free eval_result after conversion to a Java string!
                return str_result;
            },
            expr);
        CTypeConversion.CCharPointerHolder holder = CTypeConversion.toCString(result);
        CCharPointer value = holder.get();
        return value;
    }
}
