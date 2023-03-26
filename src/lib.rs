#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use emacs::{defun, Env, IntoLisp }; //, Value

emacs::plugin_is_GPL_compatible!();

use std::ffi::{CStr, CString, c_void};
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

fn eval(env: &Env, expr: String) -> String {
    let ptr: *mut *const c_void = unsafe { mem::transmute(env) };
    unsafe {
	    let cexpr = CString::new(expr).expect("CString::new failed");
        let result = eval_string(
            thread as i64,
            ptr,
            cexpr.as_ptr(),
        );
        let s = my_string_safe(result).unwrap();
        s
    }
}

#[emacs::module(name = "scimacs")]
fn init(_env: &Env) -> emacs::Result<()> {
    unsafe {
        graal_create_isolate(ptr::null_mut(), &mut isolate, &mut thread);
    }
    Ok(())
}

#[defun]
fn eval_sci(env: &Env, sexp: String) -> emacs::Result<String>
{
    Ok(eval(env, sexp.to_owned()).to_owned())
}

// TODO
// - params should be a vector of parameters
// - doesn't return anything to sci, should return an EDN string
// - the emacs side should read-from-string each of the params
#[no_mangle]
pub extern "C" fn eval_in_emacs(env : &Env, func : *const i8, params : *const i8)
{
    // combine these into a single vector of fn + args?
    let func_str: &CStr = unsafe { CStr::from_ptr(func) };
    let func_slice: &str = func_str.to_str().unwrap();

    let params_str: &CStr = unsafe { CStr::from_ptr(params) };
    let params_slice: &str = params_str.to_str().unwrap();

    // should wrap this in an elisp function that prints the result as
    // an EDN string.
    let _ret = env.call(func_slice, (params_slice,));
}

// experimental stuff I'll use later...

// env.call("add-hook", [
//     env.intern("text-mode-hook")?,
//     env.intern("variable-pitch-mode")?,
// ])?;

// let int_type = env.type_of(5.into_lisp(env).unwrap()).unwrap();
// let same = env.eq(int_type, env.intern("integer").unwrap());
// println!("{:?}", same);

// let _res = env.call("message", (
//     "%s: %s",
//     unsafe { symbol.value(&env) },
//     unsafe { data.value(&env) },
// ));
