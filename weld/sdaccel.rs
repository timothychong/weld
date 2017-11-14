use easy_sda;

use super::ast::*;
use super::ast::Type::*;
use super::conf::ParsedConf;
use super::CompilationStats;
use super::error::*;
use super::program::Program;

// Function name in library
const FUNC_NAME: &'static str = "test_run_method_name";

/// A compiled module holding the generated SDAccel module and some additional
/// information (e.g., the parameter and return types of the module).
pub struct CompiledModule {
    llvm_module: easy_sda::CompiledModule,
    param_types: Vec<Type>,
    return_type: Type,
}

impl CompiledModule {
    /// Returns a mutable reference to the LLVM module.
    pub fn llvm_mut(&mut self) -> &mut easy_sda::CompiledModule {
        &mut self.llvm_module
    }

    /// Returns the parameter types of the module.
    pub fn param_types(&self) -> &Vec<Type> {
        &self.param_types
    }

    /// Returns the return type of the module.
    pub fn return_type(&self) -> &Type {
        &self.return_type
    }
}

/// Generate a compiled LLVM module from a program whose body is a function.
pub fn compile_program(program: &Program, conf: &ParsedConf, stats: &mut CompilationStats)
        -> WeldResult<CompiledModule> {

    debug!("Started compiling LLVM");
    let module = easy_sda::compile_module(FUNC_NAME);
    debug!("Done compiling LLVM");

    Ok(CompiledModule {
        llvm_module: module.unwrap(),
        param_types: Vec::new(), //TODO: Dennis, made something up to satisfy interface
        return_type: Scalar(ScalarKind::I8), //TODO: Dennis, made something up to satisfy interface
    })
}