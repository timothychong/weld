use super::ast::*;
use super::error::*;
use super::passes::*;
use super::pretty_print::*;
use super::transforms;
use super::type_inference;
use super::macro_processor;
use super::program::Program;
use super::util::SymbolGenerator;
use super::code_builder::*;

use super::sdaccel_util::*;
use super::sdaccel_type::*;

//Writing OpenCl output to file
use std::fs::File;
use std::io::prelude::*;


// Placeholder
pub struct SDAccelProgram {
    pub sym_gen: SymbolGenerator,
    pub ret_ty: Type,
    pub top_params: Vec<TypedParameter>,
    main: CodeBuilder,
    world: SDAccelVar,
    dealloc: CodeBuilder,
}

impl SDAccelProgram {
    pub fn new(ret_ty: &Type, top_params: &Vec<TypedParameter>) -> SDAccelProgram {
        let mut generator = SymbolGenerator::new();
        let world_sym = generator.new_symbol(
            &sym_key_from_sdacceltype(SDAccelType::XCLWorld));
        let mut prog = SDAccelProgram {
            ret_ty: ret_ty.clone(),
            top_params: top_params.clone(),
            sym_gen: generator,
            main: CodeBuilder::new(),
            dealloc: CodeBuilder::new(),
            world: SDAccelVar{
                sym: world_sym,
                ty: SDAccelType::XCLWorld,
            }
        };
        /// add main
        prog
    }
    pub fn new_cl_var(&mut self, ty:SDAccelType) -> SDAccelVar {
        SDAccelVar {
            sym: self.sym_gen.new_symbol(&sym_key_from_sdacceltype(ty)),
            ty: ty,
        }
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

    pub fn allocate_buffer(&mut self) -> WeldResult<()> {
        for param in &self.top_params.clone() {
            match param.ty {
                Type::Vector(_) =>  {
                    self.main.add_line(gen_line_size_in_byte(&param));
                    let mut cl = self.new_cl_var(SDAccelType::CLInt);
                    self.main.add_line(cl.gen_declare());
                    let mut mem_var = self.new_cl_var(SDAccelType::CLMem);

                    self.main.add_line(gen_line_buffer_mem(&param, &mut cl,
                                                           &mut mem_var));
                }
                _ => {}
            }

        }
        Ok(())
    }

    pub fn get_kernel(&mut self) -> WeldResult<()> {
        let mut program = self.new_cl_var(SDAccelType::CLProgram);
        let mut kernel = self.new_cl_var(SDAccelType::CLKernel);

        self.main.add_line(
            assign(
                program.gen_var(),
                SDAccelFuncBuilder {
                    ty: SDAccelFuncType::XCLImportBinary,
                    args: vec![
                    self.world.gen_name(),
                    SDACCEL_MAIN_PROGRAM.to_string(),
                    ]
                }.emit()
            )
        );

        self.main.add_line(kernel.gen_declare_assign_str("0"));

        self.main.add_line(
            assign(
                kernel.gen_name(),
                SDAccelFuncBuilder {
                    ty: SDAccelFuncType::XCLGetKernel,
                    args: vec![
                        program.gen_var(),
                        SDACCEL_MAIN_KERNEL.to_string(),
                    ]
                }.emit()
            )
        );
        Ok(())
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
        -> WeldResult<String> {

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

    ast_to_sdaccel(&expr)
}

pub fn ast_to_sdaccel(expr: &TypedExpr) -> WeldResult<String> {
    if let ExprKind::Lambda { ref params, ref body } = expr.kind {

        let mut prog = SDAccelProgram::new(&expr.ty, params);
        prog.sym_gen = SymbolGenerator::from_expression(expr);

        let input_str = prog.gen_input_param_str().unwrap();

        let with_input = HOST_CODE.replace("$INPUTS", &input_str);

        let _buffer_res = prog.allocate_buffer();
        let _kernel_res = prog.get_kernel();

        let mut final_host = CodeBuilder::new();
        final_host.add(with_input);
        final_host.add_code(&prog.main);

        let mut file = File::create("host.cpp")?;
        println!("{}", final_host.result());
        file.write_all(final_host.result().as_bytes())?;

        Ok(String::from("TRIAL"))

    } else {
        weld_err!("Expression passed to ast_to_spatial was not a Lambda")
    }

}

