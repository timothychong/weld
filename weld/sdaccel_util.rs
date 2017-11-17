use super::ast::*;
use super::error::*;
use super::sdaccel_type::*;

// Name generation
pub const SDACCEL_ARG_SEPARATOR: &'static str = ", ";
pub const SDACCEL_ARG_POINTER: &'static str = "* ";
pub const SDACCEL_SIZE_SUFFIX: &'static str = "_size";
pub const SDACCEL_SIZE_IN_BYTES_SUFFIX: &'static str = "_size_in_byte";
pub const SDACCEL_SIZE_KIND: ScalarKind = ScalarKind::I64;
pub const SDACCEL_SIZE_TYPE: Type = Type::Scalar(SDACCEL_SIZE_KIND);
pub const SDACCEL_COUNTER_TYPE: Type = Type::Scalar(ScalarKind::I64);
pub const SDACCEL_BUFFER_MEM_SUFFIX: &'static str = "_mbuf";
pub const SDACCEL_BUFFER_BUILD_PREFIX: &'static str = "build_buff_";


pub const SDACCEL_RESULT_PREFIX: &'static str = "result_";


pub const SDACCEL_MAIN_PROGRAM: &'static str = "\"main_program\"";
pub const SDACCEL_MAIN_KERNEL: &'static str = "\"func_0\"";

pub static HOST_CODE: &'static str = include_str!("resources-sdaccel/host.ll");


//result
pub const SDACCEL_RESULT_VECTOR_NAME: &'static str = "result_vector";


pub fn gen_scalar_type_from_kind(scalar_kind: &ScalarKind) -> String {
     String::from(match *scalar_kind {
        ScalarKind::Bool => "bool",
        ScalarKind::I8 => "signed char",
        ScalarKind::U8 => "unsigned char",
        ScalarKind::I32|ScalarKind::I16 => "int",
        ScalarKind::U32|ScalarKind::U16 => "unsigned int",
        ScalarKind::I64 => "long",
        ScalarKind::U64 => "unsigned long",
        ScalarKind::F32 => "float",
        ScalarKind::F64 => "double",
    })
}

pub fn gen_scalar_type(ty: &Type) -> WeldResult<String> {
    match *ty {
        Type::Scalar(scalar_kind) => Ok(gen_scalar_type_from_kind(&scalar_kind)),
        Type::Vector(ref s) => {
            match **s {
                Type::Scalar(kind) =>{
                    Ok(format!{"{} *", gen_scalar_type_from_kind(&kind)})
                }
                _ => weld_err!("Not supported gen_scalar type inside vector. {:?} ", ty)
            }
        }
        _ => weld_err!("Not supported gen_scalar type. {:?} ", ty)
    }
}

pub fn gen_var_size_in_byte(name: &String) -> String {
    let ty = Type::Scalar(SDACCEL_SIZE_KIND);
    gen_var(ty, &gen_name_size_in_byte(name)).unwrap()
}

pub fn gen_var_size_in_byte_typed(param: &TypedParameter) -> String {
    let mut myparam = param.clone();
    myparam.name.name = gen_name_size_in_byte_typed(param);
    myparam.ty = Type::Scalar(SDACCEL_SIZE_KIND);
    gen_var_typed(&myparam).unwrap()
}

pub fn gen_line_size_in_byte_buff(ty: Type, name: &String, target: &String) -> String {
    assign( gen_var_size_in_byte(name), gen_size_in_byte(ty, Some(target)).unwrap())
}

pub fn gen_line_size_in_byte_var(ty: Type, name: &String) -> String {
    assign( gen_var_size_in_byte(name), gen_size_in_byte(ty, None).unwrap())
}

pub fn gen_line_size_in_byte(ty: Type, name: &String) -> String {
    assign( gen_var_size_in_byte(name), gen_size_in_byte(ty, Some(name)).unwrap())
}

pub fn gen_line_size_in_byte_typed(param: &TypedParameter) -> String {
    assign( gen_var_size_in_byte_typed(param), gen_size_in_byte_typed(param).unwrap())
}

pub fn gen_sym_size_sym(sym: &Symbol) -> Symbol {
    Symbol {
        name: format!("{}{}", sym.name, SDACCEL_SIZE_SUFFIX),
        id: sym.id
    }
}

pub fn gen_name_buffer_mem(param: &TypedParameter) -> String {
    let name = &param.name.name;
    format!("{}{}", name, SDACCEL_BUFFER_MEM_SUFFIX)
}
pub fn gen_line_buffer_mem(param: &TypedParameter, cl: &mut SDAccelVar,
                           mem_var: &mut SDAccelVar,
                           world: &mut SDAccelVar) -> String {

    let mut func = SDAccelFuncBuilder {
        ty: SDAccelFuncType::CLCreateBuffer,
        args: vec![
        world.gen_var_attr("context"),
        "CL_MEM_COPY_HOST_PTR".to_string(),
        gen_name_size_in_byte_typed(param),
        gen_name(param),
        cl.gen_ref()
        ]
    };
    assign(mem_var.gen_var(), func.emit())
}

//pub fn gen_line_alloc_output_buffer_mem(mem_var: &mut SDAccelVar,  index: usize, world: &mut SDAccelVar) -> String {

    //let mut func = SDAccelFuncBuilder {
        //ty: SDAccelFuncType::XCLMalloc,
        //args: vec![
        //world.gen_name(),
        //"CL_MEM_WRITE_ONLY".to_string(),
        //gen_name_size_in_byte(&gen_name_result(index)),
        //]
    //};
    //assign(mem_var.gen_name(), func.emit())

//}

pub fn gen_line_alloc_output_buffer_mem_builder(mem_var: &mut SDAccelVar,
                                                world: &mut SDAccelVar) -> String {

    let mut func = SDAccelFuncBuilder {
        ty: SDAccelFuncType::XCLMalloc,
        args: vec![
        world.gen_name(),
        "CL_MEM_READ_WRITE".to_string(),
        gen_name_size_in_byte(&mem_var.gen_name()),
        ]
    };
    assign(mem_var.gen_name(), func.emit())

}


pub fn gen_name_size_in_byte_typed(param: &TypedParameter) -> String {
    gen_name_size_in_byte(&param.name.name)
}

pub fn gen_name_size_in_byte(name: &String) -> String {
    format!("{}{}", name, SDACCEL_SIZE_IN_BYTES_SUFFIX)
}

pub fn assign(lhs: String, rhs: String) -> String {
    format!("{} = {};", lhs, rhs)
}

pub fn gen_sizeof(x:String) -> String {
    format!("sizeof( {} )", x)
}

pub fn gen_size_in_byte_typed(param: &TypedParameter) -> WeldResult<String> {
    let name = &param.name.name;
    gen_size_in_byte(param.ty.clone(), Some(name))
}


pub fn gen_size_in_byte(ty: Type, name: Option<&String>) -> WeldResult<String> {

    match name {
        Some(name) => {
            match ty {
                Type::Vector(ref boxx) => {
                    let size_name = gen_name_size(name);
                    let kind_name = gen_scalar_type(boxx).unwrap();
                    Ok(format!("{} * {}", size_name, gen_sizeof(kind_name)))
                }
                _ => weld_err!("Not supported result type for gen_size_in_type.")
            }
        }
        None => {
            Ok(format!("{}", gen_sizeof(gen_scalar_type(&ty)?)))
        }

    }
}

pub fn gen_name(param: &TypedParameter) -> String {
    param.name.name.clone()
}


pub fn gen_var(ty: Type, name: &String) -> WeldResult<String> {
    match ty {
        Type::Scalar(scalar_kind) => {
            let s = format!("{} {}",
                    gen_scalar_type_from_kind(&scalar_kind),
                    name);
            Ok(s)
        },
        Type::Vector(ref boxx) => {
            // Actual vector
            let vec_arg = format!("{}{}{}", gen_scalar_type(boxx).unwrap(),
                SDACCEL_ARG_POINTER,
                name
            );
            Ok(vec_arg)
        },
        _ => return weld_err!("Not supported result type for gen_var.")
    }
}


pub fn gen_var_typed(param: &TypedParameter) -> WeldResult<String> {
    let name = &param.name.name;
    gen_var(param.ty.clone(), name)
}

pub fn gen_name_size(name: &String) -> String {
    format!("{}{}", name, SDACCEL_SIZE_SUFFIX)
}

pub fn gen_arg(ty: Type, name: &String) -> WeldResult<Vec<String>> {
    let mut result = Vec::new();

    match ty {
        Type::Scalar(_) => {
            result.push(gen_var(ty, name).unwrap());
        },
        Type::Vector(_) => {
            //// Actual vector
            result.push(gen_var(ty, name).unwrap());
            result.push(gen_var(Type::Scalar(SDACCEL_SIZE_KIND), &gen_name_size(name)).unwrap());
        },
        _ => return weld_err!("Not supported result type for gen_arg."),
    };
    Ok(result)

}

pub fn gen_arg_type(param: &TypedParameter) -> WeldResult<Vec<String>>{
    let name = &param.name.name;
    gen_arg(param.ty.clone(), name)
}

pub fn get_sdaccel_type_from_kind(scalar_kind: ScalarKind) -> SDAccelType {
    match scalar_kind {
         // Just use cl_int for everything for now
        ScalarKind::I64 => SDAccelType::CLLong,
         _ => SDAccelType::CLInt
    }
}


pub fn gen_set_arg(buffer_sym: Symbol, origin_param: &TypedParameter, kernel: SDAccelVar,
                   buffer: bool) -> WeldResult<(String, Option<String>)> {
    let mut vars = Vec::new();
    let mut result = Vec::new();
    match  origin_param.ty{
        Type::Scalar(scalar_kind) => {
            vars.push(
                SDAccelVar {
                    sym: origin_param.name.clone(),
                    ty:get_sdaccel_type_from_kind(scalar_kind)
                }
           );
        },
        Type::Vector(_) => {
            let origin_name = origin_param.name.name.clone();
            let size = SDAccelVar {
                sym : Symbol {
                    name : gen_name_size(&origin_name),
                    id : 0
                },
                ty : SDAccelType::CLLong
            };
            //let actual_name = match buffer {
                //true => buffer_sym.name.clone(),
                //false => buffer_sym.clone()

            //};
            let mem = SDAccelVar {
                sym : buffer_sym,
                ty : SDAccelType::CLMem
            };
            //// Actual vector
            vars.push(mem);
            if !buffer {
                vars.push(size);
            }
        },
        _ => return weld_err!("Not supported vars type.")
    }
    for var in vars {
        result.push(SDAccelFuncBuilder {
            ty: SDAccelFuncType::XCLSetKernelArg,
            args: vec![
            kernel.gen_name(),
            "nargs++".to_string(),
            gen_sizeof(var.gen_typename()),
            var.gen_ref(),
            ]
        }.emit_line());

    }
    if result.len() == 2 {
        return Ok((result[0].clone(), Some(result[1].clone())))
    } else if result.len() == 1{
        return Ok((result[0].clone(), None))
    } else {
        weld_err!("Result vector has more than 2 items in set args")
    }
}

//pub fn gen_set_arg_typed(param: &TypedParameter, index: &mut i32, mut kernel: SDAccelVar)
    //-> WeldResult<Vec<String>>{
    //let name = &param.name.name;
    //gen_set_arg(param.ty.clone(), name, index, kernel)
//}

pub fn ocl_check(s:String) -> String {
    format!("OCL_CHECK({});",s)
}

pub fn gen_comment(comment: &str) -> String{
    format!("//{}", comment)
}

pub fn gen_name_result(index: usize) -> String {
    format!("{}{}", SDACCEL_RESULT_PREFIX, index)
}

pub fn gen_name_result_mem(index: usize) -> String {
    format!("mem_{}{}", SDACCEL_RESULT_PREFIX, index)
}

pub fn gen_wait_events(index: usize, events: &mut SDAccelVar) -> String {
    SDAccelFuncBuilder {
        ty: SDAccelFuncType::CLWaitForEvents,
        args: vec![
            format!("{}", index),
            events.gen_var_attr("data()"),
        ]
    }.emit_line()
}

pub fn gen_wait_event(index: usize, events: &SDAccelVar) -> String {
    SDAccelFuncBuilder {
        ty: SDAccelFuncType::CLWaitForEvent,
        args: vec![
            events.gen_ref_idx(index),
        ]
    }.emit_line()
}

pub fn get_release(e: &SDAccelVar) -> WeldResult<String> {
    let name = e.gen_name();
    match e.ty {
        SDAccelType::CLEvent => {
            let s = SDAccelFuncBuilder {
                ty: SDAccelFuncType::CLReleaseEvent,
                args: vec![name]
            }.emit_line();
            Ok(s)
        },
        SDAccelType::CLMem => {
            let s = SDAccelFuncBuilder {
                ty: SDAccelFuncType::CLReleaseMemObject,
                args: vec![name]
            }.emit_line();
            Ok(s)
        },
        SDAccelType::CLKernel => {
            let s = SDAccelFuncBuilder {
                ty: SDAccelFuncType::CLReleaseKernel,
                args: vec![name]
            }.emit_line();
            Ok(s)
        },
        SDAccelType::CLProgram => {
            let s = SDAccelFuncBuilder {
                ty: SDAccelFuncType::CLReleaseProgram,
                args: vec![name]
            }.emit_line();
            Ok(s)
        },
        SDAccelType::XCLWorld => {
            let s = SDAccelFuncBuilder {
                ty: SDAccelFuncType::XCLReleaseWorld,
                args: vec![name]
            }.emit_line();
            Ok(s)
        },
        SDAccelType::Vector(ref x, _) => {
            match **x{
                SDAccelType::CLEvent => {
                    // Hard coding for now
                    let mut s = format!("for (auto event: {})\n", &name);
                    s.push_str(&SDAccelFuncBuilder {
                        ty: SDAccelFuncType::CLReleaseEvent,
                        args: vec![
                            "event".to_string()
                        ]
                    }.emit_line());
                    Ok(s)

                }
                _ => weld_err!("Get release event wrong type")
            }
        },
        SDAccelType::CLCommandQueue => {
            let s = SDAccelFuncBuilder {
                ty: SDAccelFuncType::CLReleaseCommandQueue,
                args: vec![name]
            }.emit_line();
            Ok(s)
        },
        _ => weld_err!("Get release event wrong type")
    }

}

pub fn deref_sym(s:&Symbol) -> Symbol {
    Symbol {
        name: format!("*{}", s),
        id:s.id
    }
}
