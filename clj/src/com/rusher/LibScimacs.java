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
    // TODO should take a vector of params and return an EDN string
    // that gets read by sci
    @CFunction("eval_in_emacs")
    public static native void eval_in_emacs(@CConst WordPointer env,
                                            @CConst CCharPointer func,
                                            @CConst CCharPointer param);
    
    @CEntryPoint(name = "eval_string")
    public static @CConst CCharPointer evalString(@CEntryPoint.IsolateThreadContext long isolateId,
                                                  @CConst WordPointer env,
                                                  @CConst CCharPointer s) {
        String expr = CTypeConversion.toJavaString(s);
        String result = com.rusher.scimacs.evalString((Object func, Object param) -> {
                CTypeConversion.CCharPointerHolder func_holder = CTypeConversion.toCString((String)func);
                CTypeConversion.CCharPointerHolder param_holder = CTypeConversion.toCString((String)param);
                CCharPointer func_value = func_holder.get();
                CCharPointer param_value = param_holder.get();
                eval_in_emacs(env, func_value, param_value);
            },
            expr);
        CTypeConversion.CCharPointerHolder holder = CTypeConversion.toCString(result);
        CCharPointer value = holder.get();
        return value;
    }
}
