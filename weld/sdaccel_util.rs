use super::ast::*;
use super::error::*;


pub const SDACCEL_ARG_SEPARATOR: &'static str = ", ";
pub const SDACCEL_ARG_POINTER: &'static str = "* ";
pub const SDACCEL_SIZE_SUFFIX: &'static str = "_size";
pub const SDACCEL_SIZE_KIND: ScalarKind = ScalarKind::U32;

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

pub fn gen_arg(param: &TypedParameter) -> WeldResult<Vec<String>>{

    let mut result = Vec::new();
    let name = &param.name.name;

    match param.ty {
        Type::Scalar(scalar_kind) => {
            let s = format!("{} {}",
                    gen_scalar_type_from_kind(scalar_kind).unwrap(),
                    name);
            result.push(s);
        },
        Type::Vector(ref boxx) => {
            // Actual vector
            let size_name = format!("{}{}", name, SDACCEL_SIZE_SUFFIX);
            let size_arg = format!("{} {}",
                                   gen_scalar_type_from_kind(SDACCEL_SIZE_KIND).unwrap()
                                   , size_name);

            let vec_arg = format!("{}{}{}", gen_scalar_type(boxx).unwrap(),
                SDACCEL_ARG_POINTER,
                name
            );
            result.push(vec_arg);
            result.push(size_arg);
        },
        _ => return weld_err!("Not supported result type.")
    }
    Ok(result)

}
