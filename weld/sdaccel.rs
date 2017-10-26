use super::ast::*;
use super::error::*;
use super::passes::*;
use super::pretty_print::*;
use super::transforms;
use super::type_inference;
use super::macro_processor;
use super::program::Program;
use super::util::SymbolGenerator;

use super::sdaccel_util::*;

// Placeholder
pub struct SDAccelProgram {
    pub sym_gen: SymbolGenerator,
    pub ret_ty: Type,
    pub top_params: Vec<TypedParameter>,

    // Input params
}

impl SDAccelProgram {
    pub fn new(ret_ty: &Type, top_params: &Vec<TypedParameter>) -> SDAccelProgram {
        let mut prog = SDAccelProgram {
            ret_ty: ret_ty.clone(),
            top_params: top_params.clone(),
            sym_gen: SymbolGenerator::new(),
        };
        /// add main
        prog
    }
}

impl SDAccelProgram {


    // Input parameters related
    pub fn gen_input_param_str(&mut self) -> WeldResult<(String)>{

        let mut param_str_vec = Vec::new();

        for param in &self.top_params {
            param_str_vec.extend_from_slice(&gen_arg(param).unwrap());
        }
        let final_str = param_str_vec.join(SDACCEL_ARG_SEPARATOR);
        Ok(final_str)
    }
}

pub fn apply_opt_passes(expr: &mut TypedExpr, opt_passes: &Vec<Pass>) -> WeldResult<()> {
    for pass in opt_passes {
        pass.transform(expr)?;
        trace!("After {} pass:\n{}", pass.pass_name(), print_typed_expr(&expr));
    }
    Ok(())
}

pub fn compile_program(program: &Program, opt_passes: &Vec<Pass>)
        -> WeldResult<i32> {

    let mut expr = macro_processor::process_program(program)?;
    trace!("After macro substitution:\n{}\n", print_typed_expr(&expr));

    let _ = transforms::uniquify(&mut expr)?;
    type_inference::infer_types(&mut expr)?;
    let mut expr = expr.to_typed()?;
    trace!("After type inference:\n{}\n", print_typed_expr(&expr));

    apply_opt_passes(&mut expr, opt_passes)?;

    transforms::uniquify(&mut expr)?;
    debug!("Optimized Weld program:\n{}\n", print_expr(&expr));

    println!("Optimized Weld program:\n{}\n", print_expr(&expr));

    println!("Generating OpenCL");

    ast_to_sdaccel(&expr);
    let result = 3;

    Ok(result)
}

pub fn ast_to_sdaccel(expr: &TypedExpr) -> WeldResult<String> {
    if let ExprKind::Lambda { ref params, ref body } = expr.kind {

        let mut prog = SDAccelProgram::new(&expr.ty, params);
        prog.sym_gen = SymbolGenerator::from_expression(expr);


        println!("Input string: {}", prog.gen_input_param_str().unwrap());

        Ok(String::from("TRIAL"))

    } else {
        weld_err!("Expression passed to ast_to_spatial was not a Lambda")
    }

}

