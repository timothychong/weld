  //! A very simple wrapper for SDAccel

extern crate libc;
// recommended by Rust FAQ for dynamic library import
extern crate libloading;

use std::error::Error;
use std::ffi::{CString, NulError};
use std::fmt;
use std::result::Result;
use std::ops::Drop;

#[cfg(test)]
mod tests;


// Library of generated host code
const LIB_PATH: &'static str = "./libtest.so";

/// Error type returned by easy_sda.
#[derive(Debug)]
pub struct SdAccelError(String);

impl SdAccelError {
    pub fn new(description: &str) -> SdAccelError {
        SdAccelError(description.to_string())
    }

    pub fn to_string(self) -> String {
        self.0
    }
}

impl fmt::Display for SdAccelError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for SdAccelError {
    fn description(&self) -> &str {
        &self.0
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl From<NulError> for SdAccelError {
    fn from(_: NulError) -> SdAccelError {
        SdAccelError::new("Null byte in string")
    }
}

/// The type of function pointer we'll return. We only support functions that take and return i64.
type I64Func = extern "C" fn(i64) -> i64;

/// A compiled module returned by `compile_module`, wrapping a `run` function that takes `i64`
/// and returns `i64`.
#[derive(Debug)]
pub struct CompiledModule {
    run_function_name: CString,
}

impl CompiledModule {
    /// Call the module's `run` function, which must take and return i64.
    pub fn run(&self, arg: i64) -> i64 {
    	dynamic_call(self.run_function_name.clone(), arg)
    }
}

impl Drop for CompiledModule {
    /// Disposes of the LLVM execution engine and compiled function.
    fn drop(&mut self) {
        // free any allocated memory here
        ()
    }
}

/// Compile a string of SdAccel host code into a dynamic library that can then
/// be loaded by calling `CompiledModule::run`. Returns a `CompiledModule`.
/// TODO: Dennis, does not currently support multiple functions in the same
/// host
pub fn compile_module(func_name: &str)
        			  -> Result<CompiledModule, SdAccelError> {
    unsafe {
    	println!("Starting SDAccel module compilation...");
        // Create a CompiledModule to wrap the context and our result (will clean it on Drop).
        let mut result = CompiledModule {
            run_function_name: CString::new(func_name).unwrap(),
        };

        println!("Done creating SDAccel module with function name {}", 
        	result.run_function_name.to_str().unwrap());

        Ok(result)
    }
}

/// Loads in the dynamic library for SdAccel host code and finds a function
/// with name `func_name` and calls it with arguments `arg`. Returns the
/// return value of the function.
fn dynamic_call(func_name: CString,
				arg: i64)
				-> i64 {
	// TODO: Dennis, initialize libload once
	println!("Starting SDAccel dynamic call to {}", 
		func_name.to_str().unwrap());

	let lib = libloading::Library::new(LIB_PATH).unwrap();
	unsafe {
		let func: libloading::Symbol<I64Func> = lib.get(func_name.as_bytes()).unwrap();
		func(arg)
	}
}

