#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use emacs::{defun, Env, ErrorKind, ErrorKind::Signal };

emacs::plugin_is_GPL_compatible!();

use std::ffi::{CStr, CString, c_char, c_void};
use std::str::Utf8Error;
use std::{env, ptr, mem};

// Static handles for the graal isolate. These are initialized when
// the module is loaded.
static mut isolate: *mut graal_isolate_t = ptr::null_mut();
static mut thread: *mut graal_isolatethread_t = ptr::null_mut();

// we currently don't release this resource, but we probably should in
// the case of module unload:
// graal_tear_down_isolate(thread);

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

fn my_string_safe(ptr: *mut i8) -> Result<String,Utf8Error> {
    let s = unsafe {
        CStr::from_ptr(ptr).to_str()
    };
    match s {
        Ok(s) => Ok(String::from(s)),
        Err(x) => Err(x)
    }
}

#[emacs::module(name = "scimacs")]
fn init(env: &Env) -> emacs::Result<()> {
    unsafe {
        graal_create_isolate(ptr::null_mut(), &mut isolate, &mut thread);
    }
    env.call("set", (env.intern("scimacs-version")?, option_env!("CARGO_PKG_VERSION")))?;
    Ok(())
}

// TODO combine this and eval_string into one function?
fn eval(env: &Env, expr: String) -> String {
    unsafe {
        let ptr: *mut *const c_void = mem::transmute(env);
	    let cexpr = CString::new(expr).expect("CString::new failed");
        let result = sci_eval_string(
            thread as i64,
            ptr,
            cexpr.as_ptr(),
        );
        let s = my_string_safe(result).unwrap();
        s
    }
}

#[defun]
fn eval_string(env: &Env, sexp: String) -> emacs::Result<String>
{
    Ok(eval(env, sexp.to_owned()).to_owned())
}

#[no_mangle]
pub extern "C" fn eval_in_emacs(env : &Env, func : *const i8, params : *const i8) ->  *const c_char
{
    let func_str: &CStr = unsafe { CStr::from_ptr(func) };
    let func_slice: &str = func_str.to_str().unwrap();

    let params_str: &CStr = unsafe { CStr::from_ptr(params) };
    let params_slice: &str = params_str.to_str().unwrap();

    // this elisp function catches all errors and converts them to
    // strings, so we should never see a Err here unless the machine
    // has run out of memory.
    match env.call("scimacs-emacs-apply", (func_slice, params_slice)) {
        Ok(v) => {
            match v.into_rust::<String>() {
                Ok(s) => {
                    let c_str = CString::new(s).unwrap();    
                    let c_world: *const c_char = c_str.into_raw() as *const c_char;
                    return c_world;
                }
                Err(err) => {
                    println!("could not make Rust string from emacs string: {:?}", err);
                }
            }
        }
        Err(e) => {
            if let Some(&Signal { ref symbol, .. }) = e.downcast_ref::<ErrorKind>() {
                println!("emacs function call error: {:?}", unsafe{ symbol.value(env)});
            }
        }
    }

    // this should never happen in normal operation
    let c_str = CString::new("SOMETHING TERRIBLE HAS HAPPENED").unwrap();    
    let c_world: *const c_char = c_str.into_raw() as *const c_char;
    return c_world;
}
