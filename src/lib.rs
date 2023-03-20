#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use emacs::{defun, Env}; // , Value, IntoLisp

emacs::plugin_is_GPL_compatible!();

use std::ffi::{CStr, CString};
use std::str::Utf8Error;
use std::{env, ptr};

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

fn eval(expr: String) -> String {
    unsafe {
	    let cexpr = CString::new(expr).expect("CString::new failed");
        let result = eval_string(
            thread as i64,
            cexpr.as_ptr(),
        );
        let s = my_string_safe(result).unwrap();
        s
    }
}

#[emacs::module(name = "scimacs")]
fn init(_: &Env) -> emacs::Result<()> {
    unsafe {
        graal_create_isolate(ptr::null_mut(), &mut isolate, &mut thread);
    }
    Ok(())
}

#[defun]
fn eval_sci(_env: &Env, sexp: String) -> emacs::Result<String>
{
    // env.message(eval(sexp.to_owned()))
    let result = eval(sexp.to_owned());

    Ok(result.to_owned())
}

// #[defun]
// fn _copy_text(bundle_id: Option<String>) -> Result<Option<String>> {
//     Ok(copy_text(bundle_id, None))
// }
