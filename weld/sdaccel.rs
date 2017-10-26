

use super::ast::*;
use super::error::*;
use super::passes::*;
use super::pretty_print::*;
use super::transforms;
use super::type_inference;
use super::macro_processor;
use super::program::Program;
use super::util::SymbolGenerator;

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

pub fn gen_scalar_type_from_kind(scalar_kind: ScalarKind) -> WeldResult<(String)> {
     let output = String::from(match scalar_kind {
        ScalarKind::Bool => "bool",
        ScalarKind::I8 => "signed char",
        ScalarKind::U8 => "unsigned char",
        ScalarKind::I32|ScalarKind::I16 => "signed int",
        ScalarKind::U32|ScalarKind::U16 => "unsigned int",
        ScalarKind::I64 => "signed long",
        ScalarKind::U64 => "unsigned long",
        ScalarKind::F32 => "float",
        ScalarKind::F64 => "double",
    });
     Ok(output)
}

pub fn gen_scalar_type(ty: &Type) -> WeldResult<String> {
    match *ty {
        Type::Scalar(scalar_kind) => gen_scalar_type_from_kind(scalar_kind),
        _ => weld_err!("Not supported gen_scalar type.")
    }
}


impl SDAccelProgram {


    // Input parameters related
    pub fn gen_input_param_str(&mut self) -> WeldResult<(String)>{

        let mut param_str_vec = Vec::new();

        for param in &self.top_params {
            let mut param_str = String::from("");
            let x = match param.ty {
                Type::Scalar(scalar_kind) => gen_scalar_type_from_kind(scalar_kind),
                Type::Vector(ref boxx) => {
                    Ok(format!("{} *", gen_scalar_type(boxx).unwrap()))
                },
                _ => return weld_err!("Not supported result type.")
            }.unwrap();
            param_str.push_str(&x);
            param_str.push(' ');
            param_str.push_str(&param.name.name);
            param_str_vec.push(param_str);
        }
        let final_str = param_str_vec.join(" , ");
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

