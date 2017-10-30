use super::ast::*;
use super::error::*;


pub const SDACCEL_ARG_SEPARATOR: &'static str = ", ";
pub const SDACCEL_ARG_POINTER: &'static str = "* ";
pub const SDACCEL_SIZE_SUFFIX: &'static str = "_size";
pub const SDACCEL_SIZE_IN_BYTES_SUFFIX: &'static str = "_size_in_byte";
pub const SDACCEL_SIZE_KIND: ScalarKind = ScalarKind::U32;
pub const SDACCEL_BUFFER_MEM_SUFFIX: &'static str = "_mbuf";


pub const SDACCEL_CL_INT_SYM_KEY: &'static str = "CL_INT_KEY";

pub static HOST_CODE: &'static str = include_str!("resources-sdaccel/host.ll");

pub struct SDAccelType {
    pub sym: Symbol,
}
impl SDAccelType{
    pub fn gen_name(&mut self) -> String {
        format!("{}{}", &self.sym.name, &self.sym.id)
    }
    pub fn gen_var(&mut self, type_name: String) -> String {
        format!("{} {}", type_name, self.gen_name())
    }
    pub fn gen_declare(&mut self, type_name: String) -> String {
        format!("{};", self.gen_var(type_name))
    }
}

pub struct cl_int {
    pub sdaccel: SDAccelType,
    pub type_name: String,
}

impl cl_int {
    pub fn gen_declare(&mut self) -> String {
        self.sdaccel.gen_declare(self.type_name.clone())
    }
    pub fn gen_name(&mut self) -> String {
        self.sdaccel.gen_name()
    }
}



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
pub fn gen_line_buffer_mem(param: &TypedParameter, cl: &mut cl_int) -> String {
    format!("cl_mem {} = clCreateBuffer(world.context, \
    CL_MEM_COPY_HOST_PTR, {}, \
    {}, &{});",
    gen_name_buffer_mem(param),
    gen_name_size_in_byte(param),
    gen_name(param),
    cl.gen_name())

}

pub fn gen_name_size_in_byte(param: &TypedParameter) -> String {
    format!("{}{}", param.name.name, SDACCEL_SIZE_IN_BYTES_SUFFIX)
}

pub fn assign(lhs: String, rhs: String) -> String {
    format!("{} = {};", lhs, rhs)
}

pub fn gen_size_in_byte(param: &TypedParameter) -> WeldResult<String> {
    let name = &param.name.name;
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
    let name = &param.name.name;

    match param.ty {
        Type::Scalar(scalar_kind) => {
            result.push(gen_var(param).unwrap());
        },
        Type::Vector(ref boxx) => {
            let size = TypedParameter {
                name : Symbol {
                    name : gen_name_size(param),
                    id : 0
                },
                ty : Type::Scalar(SDACCEL_SIZE_KIND)
            };
            //// Actual vector

            let vec_arg = gen_var(param).unwrap();
            result.push(gen_var(param).unwrap());
            result.push(gen_var(&size).unwrap());
        },
        _ => return weld_err!("Not supported result type.")
    }
    Ok(result)
}
