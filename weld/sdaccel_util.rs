use super::ast::*;
use super::error::*;
use super::sdaccel_type::*;

// Name generation
pub const SDACCEL_ARG_SEPARATOR: &'static str = ", ";
pub const SDACCEL_ARG_POINTER: &'static str = "* ";
pub const SDACCEL_SIZE_SUFFIX: &'static str = "_size";
pub const SDACCEL_SIZE_IN_BYTES_SUFFIX: &'static str = "_size_in_byte";
pub const SDACCEL_SIZE_KIND: ScalarKind = ScalarKind::U32;
pub const SDACCEL_BUFFER_MEM_SUFFIX: &'static str = "_mbuf";


pub const SDACCEL_MAIN_PROGRAM: &'static str = "main_program";
pub const SDACCEL_MAIN_KERNEL: &'static str = "main_kernel";

pub static HOST_CODE: &'static str = include_str!("resources-sdaccel/host.ll");


pub fn gen_scalar_type_from_kind(scalar_kind: ScalarKind) -> String {
     String::from(match scalar_kind {
        ScalarKind::Bool => "bool",
        ScalarKind::I8 => "signed char",
        ScalarKind::U8 => "unsigned char",
        ScalarKind::I32|ScalarKind::I16 => "signed int",
        ScalarKind::U32|ScalarKind::U16 => "unsigned int",
        ScalarKind::I64 => "signed long",
        ScalarKind::U64 => "unsigned long",
        ScalarKind::F32 => "float",
        ScalarKind::F64 => "double",
    })
}

pub fn gen_scalar_type(ty: &Type) -> WeldResult<String> {
    match *ty {
        Type::Scalar(scalar_kind) => Ok(gen_scalar_type_from_kind(scalar_kind)),
        _ => weld_err!("Not supported gen_scalar type.")
    }
}

pub fn gen_var_size_in_byte(param: &TypedParameter) -> String {
    let mut myparam = param.clone();
    myparam.name.name = gen_name_size_in_byte(param);
    myparam.ty = Type::Scalar(SDACCEL_SIZE_KIND);
    gen_var(&myparam).unwrap()
}

pub fn gen_line_size_in_byte(param: &TypedParameter) -> String {
    assign( gen_var_size_in_byte(param), gen_size_in_byte(param).unwrap())
}

pub fn gen_name_buffer_mem(param: &TypedParameter) -> String {
    let name = &param.name.name;
    format!("{}{}", name, SDACCEL_BUFFER_MEM_SUFFIX)
}
pub fn gen_line_buffer_mem(param: &TypedParameter, cl: &mut SDAccelVar,
                           mem_var: &mut SDAccelVar) -> String {

    let mut func = SDAccelFuncBuilder {
        ty: SDAccelFuncType::CLCreateBuffer,
        args: vec![
        "world.context".to_string(),
        "CL_MEM_COPY_HOST_PTR".to_string(),
        gen_name_size_in_byte(param),
        gen_name(param),
        format!("&{}",cl.gen_name())
        ]
    };
    assign(mem_var.gen_var(), func.emit())
}

pub fn gen_name_size_in_byte(param: &TypedParameter) -> String {
    format!("{}{}", param.name.name, SDACCEL_SIZE_IN_BYTES_SUFFIX)
}

pub fn assign(lhs: String, rhs: String) -> String {
    format!("{} = {};", lhs, rhs)
}

pub fn gen_size_in_byte(param: &TypedParameter) -> WeldResult<String> {
    match param.ty {
        Type::Vector(ref boxx) => {
            let size_name = gen_name_size(param);
            let kind_name = gen_scalar_type(boxx).unwrap();
            Ok(format!("{} * sizeof( {} )", size_name, kind_name))
        }
        _ => weld_err!("Not supported result type for gen_size_in_type.")
    }
}

pub fn gen_name(param: &TypedParameter) -> String {
    param.name.name.clone()
}

pub fn gen_var(param: &TypedParameter) -> WeldResult<String> {
    let name = &param.name.name;
    match param.ty {
        Type::Scalar(scalar_kind) => {
            let s = format!("{} {}",
                    gen_scalar_type_from_kind(scalar_kind),
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
        _ => return weld_err!("Not supported result type.")
    }
}

pub fn gen_name_size(param: &TypedParameter) -> String {
    format!("{}{}", param.name.name, SDACCEL_SIZE_SUFFIX)
}

pub fn gen_arg(param: &TypedParameter) -> WeldResult<Vec<String>>{
    let mut result = Vec::new();

    match param.ty {
        Type::Scalar(_) => {
            result.push(gen_var(param).unwrap());
        },
        Type::Vector(_) => {
            let size = TypedParameter {
                name : Symbol {
                    name : gen_name_size(param),
                    id : 0
                },
                ty : Type::Scalar(SDACCEL_SIZE_KIND)
            };
            //// Actual vector

            result.push(gen_var(param).unwrap());
            result.push(gen_var(&size).unwrap());
        },
        _ => return weld_err!("Not supported result type.")
    }
    Ok(result)
}
