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
use super::sdaccel_kernel::*;

//Writing OpenCl output to file
use std::fs::File;
use std::io::prelude::*;

use super::conf::ParsedConf;
use super::CompilationStats;
use time::PreciseTime;
use super::transforms::uniquify;

use std::collections::{BTreeMap, HashMap};

pub struct SDAccelProgram {

    // HOST
    pub sym_gen: SymbolGenerator,
    pub ret_ty: Vec<Type>,
    pub ret_ty_cl: Vec<SDAccelVar>,
    pub top_params: Vec<TypedParameter>,
    pub top_params_cl: Vec<SDAccelVar>,
    pub output_buf: Vec<SDAccelVar>,
    main: CodeBuilder,


    pub map_var_buff:HashMap<Symbol, SDAccelVar>,

    // For releasing
    events: Vec<SDAccelVar>,
    command_queue: SDAccelVar,
    world: SDAccelVar,
    kernel: SDAccelVar,
    program: SDAccelVar,

}

pub fn gen_new_cl_var(generator: &mut SymbolGenerator, ty: SDAccelType) ->
    SDAccelVar {
    let sym = generator.new_symbol(&sym_key_from_sdacceltype(ty.clone()));
    SDAccelVar{
        sym: sym,
        ty: ty,
    }
}

pub fn gen_new_cl_var_name(generator: &mut SymbolGenerator, ty: SDAccelType, name:&String) ->
    SDAccelVar {
    let sym = generator.new_symbol(&name);
    SDAccelVar{
        sym: sym,
        ty: ty,
    }
}

impl SDAccelProgram {

    pub fn new(ret_ty: &Type, top_params: &Vec<TypedParameter>) -> WeldResult<SDAccelProgram> {
        let mut generator = SymbolGenerator::new();
        let world = gen_new_cl_var( &mut generator, SDAccelType::XCLWorld);
        let kernel = gen_new_cl_var( &mut generator, SDAccelType:: CLKernel);
        let commandq = gen_new_cl_var( &mut generator, SDAccelType:: CLCommandQueue);
        let program = gen_new_cl_var( &mut generator, SDAccelType::CLProgram);
        let mut prog = SDAccelProgram {
            ret_ty: Vec::new(),
            ret_ty_cl: Vec::new(),
            top_params: top_params.clone(),
            top_params_cl: Vec::new(),
            output_buf: Vec::new(),
            sym_gen: generator,
            main: CodeBuilder::new(),
            command_queue: commandq,
            events: Vec::new(),
            world: world,
            program: program,
            kernel: kernel,
            map_var_buff: HashMap::new(),
        };
        match *ret_ty {
            Type::Function(_, ref rt) => prog.ret_ty.push(*rt.clone()),
            Type::Struct(ref v) => prog.ret_ty.extend_from_slice(&v),
            _ => return weld_err!("Unsupported function return type")
        };
        /// add main
        Ok(prog)
    }
    pub fn new_cl_var(&mut self, ty:SDAccelType) -> SDAccelVar {
        gen_new_cl_var(&mut self.sym_gen, ty)
    }

    pub fn new_cl_var_name(&mut self, ty:SDAccelType, name:&String) -> SDAccelVar {
        gen_new_cl_var_name(&mut self.sym_gen, ty, name)
    }
}

impl SDAccelProgram {

    // Input parameters related
    pub fn gen_input_output_param_str(&mut self) -> WeldResult<(String)>{

        let mut vector_kinds = Vec::new();
        let mut vector_structs = String::new();

        for param in &self.top_params {
            if let Type::Vector(ref v) = param.ty {
                if let Type::Scalar(k) = **v {
                    if let None = vector_kinds.iter().position(|x| *x == k) {
                        vector_kinds.push(k.clone());
                        let kind = gen_scalar_type_from_kind(&k);

                        vector_structs.push_str(
                            &format!(
                            "struct weld_vector_{}_t {{\n    {} *data;\n    int64_t length;\n}};",
                                        kind, kind)
                        );
                    }
                } else {
                    return weld_err!("Top param vector must be scalar kind");
                }
            }
        }

        self.main.add_line("struct args_t {");
            // Generating struct variables
            for param in &self.top_params {
                match param.ty {
                    Type::Scalar(ref k) => {
                        self.main.add_line(format!("    {} {};", gen_scalar_type_from_kind(k), &param.name));
                    },
                    Type::Vector(ref v) => {
                        if let Type::Scalar(k) = **v {
                            self.main.add_line(format!("    struct weld_vector_{}_t {};", gen_scalar_type_from_kind(&k),
                                                        &param.name));
                        }
                    },
                    _ => return weld_err!("Only scalar and vector param type is supported"),

                }
            }
        self.main.add_line("};");
        self.main.add_line("struct args_t *args = (struct args_t *)(((input_arg_t *)in)->data);");


        for param in &self.top_params {
            match param.ty {
                Type::Scalar(ref k) => {
                    self.main.add_line(format!("    {} {} = args->{};", gen_scalar_type_from_kind(k), &param.name,
                                                                &param.name));
                },
                Type::Vector(ref v) => {
                    if let Type::Scalar(k) = **v {
                        //For size
                        let size_sym = gen_sym_size_sym(&param.name);
                        self.main.add_line(format!("{} {} = args->{}.length;",
                                            gen_scalar_type_from_kind(&SDACCEL_SIZE_KIND), &size_sym, &param.name));

                        //For actual data
                        self.main.add_line(format!("{} * {} = args->{}.data;",
                                            gen_scalar_type_from_kind(&k), &param.name, &param.name));
                    }
                },
                _ => return weld_err!("Only scalar and vector param type is supported"),

            }
        }

        for param in &self.ret_ty {

        }

        // For outputs

        Ok(vector_structs)

    }

    pub fn init_world(&mut self) -> WeldResult<()> {
        self.main.add_line(gen_comment("Initializing World"));
        self.main.add_line(self.world.gen_declare_assign_str("xcl_world_single()"));
        self.main.add_line(
            assign(
                self.program.gen_var(),
                SDAccelFuncBuilder {
                    ty: SDAccelFuncType::XCLImportBinary,
                    args: vec![
                    self.world.gen_name(),
                    SDACCEL_MAIN_PROGRAM.to_string(),
                    ]
                }.emit()
            )
        );

        self.main.add_line(self.kernel.gen_declare_assign_str("0"));

        self.main.add_line(
            assign(
                self.kernel.gen_name(),
                SDAccelFuncBuilder {
                    ty: SDAccelFuncType::XCLGetKernel,
                    args: vec![
                        self.program.gen_name(),
                        SDACCEL_MAIN_KERNEL.to_string(),
                    ]
                }.emit()
            )
        );

        let mut err = self.new_cl_var(SDAccelType::CLInt);
        self.main.add_line(err.gen_declare());
        // Adding command queue
        self.main.add_line(
            assign(
                self.command_queue.gen_var(),
                SDAccelFuncBuilder {
                    ty: SDAccelFuncType::CLCreateCommandQueue,
                    args: vec![
                    self.world.gen_var_attr("context"),
                    self.world.gen_var_attr("device_id"),
                    "CL_QUEUE_PROFILING_ENABLE".to_string(),
                    err.gen_ref()
                    ]
                }.emit()
            )
        );
        Ok(())
    }

    pub fn allocate_buffer(&mut self) -> WeldResult<()> {
        self.main.add_line(gen_comment("Allocating Buffers"));
        // Generate lines for memory transfer from host to device
        for param in &self.top_params.clone() {
            match param.ty {
                Type::Vector(_) =>  {
                    self.main.add_line(gen_line_size_in_byte_typed(&param));
                    let mut cl = self.new_cl_var(SDAccelType::CLInt);
                    self.main.add_line(cl.gen_declare());
                    let mut mem_var = self.new_cl_var(SDAccelType::CLMem);
                    self.main.add_line(gen_line_buffer_mem(&param, &mut cl,
                                                           &mut mem_var,
                                                           &mut self.world));
                    self.map_var_buff.insert(gen_sym_size_sym(&param.name), cl.clone());
                    self.map_var_buff.insert(param.name.clone(), mem_var.clone());
                }
                _ => {}
            }

        }
        //for (i, x) in self.ret_ty.clone().iter().enumerate() {
            //match *x {
                //Type::Vector(_) =>  {
                    //let mut mem_var = self.new_cl_var(SDAccelType::CLMem);
                    //self.main.add_line(mem_var.gen_declare());
                    //self.main.add_line(
                        //gen_line_alloc_output_buffer_mem(&mut mem_var,
                                                         //i,
                                                         //&mut self.world)
                    //);
                    //self.output_buf.push(mem_var)
                //},
                //_ => {}
            //}
        //}

        // Output buffer
        Ok(())
    }

    pub fn set_kernel_buffer(&mut self, prog: & SDAccelKernel) {
        let sorted: BTreeMap<&Symbol, &Type> = prog.buffs.iter().collect();
        for (i, x) in prog.buffs.iter().enumerate() {
            let (name, ty) = x;
            let mut mem_var = self.new_cl_var_name(SDAccelType::CLMem, &name.name);
            let s = prog.find_var_target(&name);
            self.main.add_line(
                gen_line_size_in_byte_buff(
                    ty.clone(),
                    &name.name,
                    &s.name,
                    ));
            self.main.add_line(mem_var.gen_declare());
            self.main.add_line(
                gen_line_alloc_output_buffer_mem_builder(&mut mem_var,
                                                 i,
                                                 &mut self.world)
            );
            self.map_var_buff.insert(name.clone(), mem_var.clone());
        }
    }

    pub fn set_args(&mut self, prog: &SDAccelKernel) {
        self.main.add_line("int nargs = 0;".to_string());
        // Pring out variables first then sizes
        let mut sizes = Vec::new();
        for param in &self.top_params {
            let buff = self.map_var_buff.get(&param.name).unwrap().clone();
            let (mem, size) = gen_set_arg(
                    buff.sym,
                    &param,
                    self.kernel.clone(),
                    false
                    ).unwrap();
            self.main.add_line(mem);
            if let Some(s) = size {
                sizes.push(s);
            }
        }
        for s in sizes {
            self.main.add_line(s);
        }

        // Buffers
        let sorted: BTreeMap<&Symbol, &Type> = prog.buffs.iter().collect();
        for (i, x) in sorted.iter().enumerate() {
            let (name, ty) = x;

            let p = TypedParameter {
                ty: (*ty).clone(),
                name: (*name).clone(),
            };
            let (mem, size) = gen_set_arg(
                    (*name).clone(),
                    &p,
                    self.kernel.clone(),
                    true
                    ).unwrap();
            self.main.add_line(mem);
            if let Some(s) = size {
                self.main.add_line(s);
            }
        }

    }

    pub fn declare_work_size(&mut self) {
        //Hard coding for now TODO
        self.main.add_line("size_t global[1] = {1};");
        self.main.add_line("size_t local[1] = {1};");
    }

    pub fn enqueue_command(&mut self) {
        let mut kernel_event = self.new_cl_var(SDAccelType::CLEvent);
        self.main.add_line(kernel_event.gen_declare());
        self.main.add_line(
            OCL_CHECK(
            SDAccelFuncBuilder {
                ty: SDAccelFuncType::CLEnqueueNDRangeKernel,
                args: vec![
                    self.command_queue.gen_name(),
                    self.kernel.gen_name(),
                    "1".to_string(), // Hard coding for now
                    "NULL".to_string(),
                    "global".to_string(),
                    "local".to_string(),
                    "0".to_string(),
                    "NULL".to_string(),
                    kernel_event.gen_ref(),
                ]
            }.emit())
        );
        self.main.add_line(
            SDAccelFuncBuilder {
                ty: SDAccelFuncType::SetCallback,
                args: vec![
                    kernel_event.gen_name(),
                    "\"kernel\"".to_string()
                ]
            }.emit_line()
        );
        self.events.push(kernel_event);
    }

    pub fn enqueue_buffer_read(&mut self, build_buffer: &Symbol,
                               ty: &Type, size_sym: &Symbol,
                               ) -> WeldResult<()>{


        //Global output structure
        self.main.add_line("struct output_result_t *output_result = (struct output_result_t *) malloc(sizeof(struct output_result_t));");
        self.main.add_line("output_result->error_no = 0;");
        self.main.add_line("output_result->run_id = 0;");

        // Add output struct
        self.main.add_line("struct results_t {");
        let mut result_index = 0;
        for r in &self.ret_ty {
            match *r {
                Type::Scalar(k) => {

                }

                Type::Vector(ref v) => {
                    if let Type::Scalar(k) = **v {
                        self.main.add_line(format!("    struct weld_vector_{}_t {};", gen_scalar_type_from_kind(&k), gen_name_result(result_index)));

                        result_index += 2;
                    } else {
                        return weld_err!("not supported: Vector inner type must be scalar")
                    }
                }
                _ => return weld_err!("Only scalar and vector return param type is supported"),
            }
        }

        self.main.add_line("};");

        self.main.add_line("struct results_t *result = (struct results_t *)malloc(sizeof(struct results_t));");
        self.main.add_line("output_result->data = (int64_t) result;");

        let num = self.ret_ty.clone().len();
        let mut events_cl = self.new_cl_var(
            SDAccelType::Vector(Box::new((SDAccelType::CLEvent)), num));
        self.main.add_line(events_cl.gen_declare());
        let buff = self.map_var_buff.get(&build_buffer).unwrap().clone();

        result_index = 0;
        match *ty {
            Type::Vector(ref v) =>  {
                // Allocating result buffer
                //

                if let Type::Scalar(k) = **v {
                    self.main.add_line(
                        &format!(
                        "result->{}.data = ({} *) malloc({});",
                        gen_name_result(result_index),
                        gen_scalar_type_from_kind(&k),
                        gen_name_size_in_byte(&build_buffer.name)
                         )
                    );
                }  else {
                    return weld_err!("not supported: Vector inner type must be scalar");
                }
                self.main.add_line(
                    OCL_CHECK(
                    SDAccelFuncBuilder {
                        ty: SDAccelFuncType::CLEnqueueReadBuffer,
                        args: vec![
                            self.command_queue.gen_name(),
                            build_buffer.clone().name,
                            "CL_TRUE".to_string(),
                            "0".to_string(),
                            gen_name_size_in_byte(
                                &build_buffer.name),
                            format!("result->{}.data", gen_name_result(result_index)),
                            "0".to_string(),
                            "NULL".to_string(),
                            events_cl.gen_ref_idx(0)
                        ]
                    }.emit())
                );
                //TODO: update to double check result_index
            }
            // TODO
            Type::Scalar(_) => {
                return weld_err!("enqueuebuffer_read not implmeneted yet");
            },
            _ => {
                return weld_err!("return type not exist");
            }
        }
        //Waiting for result transfer
        self.main.add_line(gen_wait_event(num, &mut events_cl));
        self.events.push(events_cl);
        Ok(())
    }

    pub fn release(&mut self) {
        // Release all events
        let mut string:Vec<String>  = Vec::new();

        for e in self.events.clone() {
            string.push( get_release(&e).unwrap());
        }
        string.push(get_release(&self.command_queue).unwrap());
        string.push(get_release(&self.kernel).unwrap());
        string.push(get_release(&self.program).unwrap());
        string.push(get_release(&self.world).unwrap());

        self.main.add_line(string.join("\n"));
    }
}

pub fn apply_opt_passes(expr: &mut TypedExpr, opt_passes: &Vec<Pass>, stats: &mut CompilationStats) -> WeldResult<()> {
    for pass in opt_passes {
        let start = PreciseTime::now();
        pass.transform(expr)?;
        let end = PreciseTime::now();
        stats.pass_times.push((pass.pass_name(), start.to(end)));
        trace!("After {} pass:\n{}", pass.pass_name(), print_typed_expr(&expr));
    }
    Ok(())
}

pub fn compile_program(program: &Program, conf: &ParsedConf, stats: &mut CompilationStats)
        -> WeldResult<()> {

    let mut expr = macro_processor::process_program(program)?;
    trace!("After macro substitution:\n{}\n", print_typed_expr(&expr));

    let start = PreciseTime::now();
    uniquify::uniquify(&mut expr)?;
    let end = PreciseTime::now();

    let mut uniquify_dur = start.to(end);

    let start = PreciseTime::now();
    type_inference::infer_types(&mut expr)?;
    let mut expr = expr.to_typed()?;
    trace!("After type inference:\n{}\n", print_typed_expr(&expr));
    let end = PreciseTime::now();
    stats.weld_times.push(("Type Inference".to_string(), start.to(end)));

    apply_opt_passes(&mut expr, &conf.optimization_passes, stats)?;

    let start = PreciseTime::now();
    uniquify::uniquify(&mut expr)?;
    let end = PreciseTime::now();
    uniquify_dur = uniquify_dur + start.to(end);

    stats.weld_times.push(("Uniquify outside Passes".to_string(), uniquify_dur));

    println!("Generating OpenCL");

    let start = PreciseTime::now();
    ast_to_sdaccel(&expr);
    let end = PreciseTime::now();
    stats.weld_times.push(("AST to SDACCEL".to_string(), start.to(end)));
    Ok(())
}

pub fn ast_to_sdaccel(expr: &TypedExpr) -> WeldResult<String> {
    if let ExprKind::Lambda { ref params, ref body } = expr.kind {

        let mut kernel_prog = SDAccelKernel::new();
        let first_block = kernel_prog.funcs[0].add_block();
        for tp in params {
            kernel_prog.funcs[0].params.insert(tp.name.clone(), tp.ty.clone());
        }
        //Need to update to support struct
        let sym = gen_expr(body, &mut kernel_prog, 0, first_block)?.unwrap().clone();
        let ty = kernel_prog.buffs.get(&sym).unwrap().clone();
        let sym_size = kernel_prog.find_var_target(&sym);

        let mut prog = SDAccelProgram::new(&expr.ty, params).unwrap();
        prog.sym_gen = SymbolGenerator::from_expression(expr);


        let vector_structs = prog.gen_input_output_param_str()?;
        prog.main.add_line("");

        let _world_res = prog.init_world();
        prog.main.add_line("");
        let _buffer_res = prog.allocate_buffer();
        prog.main.add_line("");
        let _set_kernel_buff_res = prog.set_kernel_buffer(&kernel_prog);
        prog.main.add_line("");
        //let _kernel_res = prog.get_kernel_and_queue();
        //prog.main.add_line("");
        let _args_res = prog.set_args(&kernel_prog);
        prog.main.add_line("");
        let _work_size_res = prog.declare_work_size();
        prog.main.add_line("");
        let _enqcommand_res = prog.enqueue_command();
        prog.main.add_line("");
        let _enqbuffread_res = prog.enqueue_buffer_read(&sym, &ty, &sym_size);
        prog.main.add_line("");
        let _release_res = prog.release();
        prog.main.add_line("return (int64_t) output_result; \n}");

        let template = HOST_CODE.replace("#WELD_VECTORS#", &vector_structs);

        kernel_prog.update_all_func_size_param();

        //println!("kernel prog\n:{}", kernel_prog);

        let mut final_host = CodeBuilder::new();
        final_host.add_code(&prog.main);

        let mut file = File::create("host.cpp")?;
        let mut kernel_file = File::create("device.cl")?;
        //println!("{}", template);
        file.write_all(template.as_bytes())?;
        file.write_all(final_host.result().as_bytes())?;
        kernel_file.write_all(format!("{}", kernel_prog).as_bytes())?;

        Ok(String::from("TRIAL"))

    } else {
        weld_err!("Expression passed to ast_to_spatial was not a Lambda")
    }

}
