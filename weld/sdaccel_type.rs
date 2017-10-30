use super::ast::*;
use super::sdaccel_util::*;
use std::fmt;

pub const SDACCEL_CL_INT_SYM_KEY: &'static str = "CL_INT_KEY";
pub const SDACCEL_CL_PROGRAM_SYM_KEY: &'static str = "CL_PROGRAM_KEY";
pub const SDACCEL_CL_KERNEL_SYM_KEY: &'static str = "CL_KERNEL_KEY";
pub const SDACCEL_CL_MEM_SYM_KEY: &'static str = "CL_MEM_KEY";
pub const SDACCEL_CL_WORLD_SYM_KEY: &'static str = "CL_WORLD_KEY";

#[derive(Clone, Copy)]
pub enum SDAccelType {
    CLInt,
    CLProgram,
    CLKernel,
    CLMem,
    XCLWorld,
}

impl fmt::Display for SDAccelType{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SDAccelType::CLInt => write!(f, "cl_int"),
            SDAccelType::CLProgram => write!(f, "cl_program"),
            SDAccelType::CLKernel => write!(f, "cl_kernel"),
            SDAccelType::CLMem => write!(f, "cl_mem"),
            SDAccelType::XCLWorld => write!(f, "xcl_world"),
        }

    }
}

pub enum SDAccelFuncType {
    CLCreateBuffer,
    XCLImportBinary,
    XCLGetKernel,

}

impl fmt::Display for SDAccelFuncType{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SDAccelFuncType::CLCreateBuffer => write!(f, "clCreateBuffer"),
            SDAccelFuncType::XCLImportBinary => write!(f, "xcl_import_binary"),
            SDAccelFuncType::XCLGetKernel => write!(f, "xcl_get_kernel"),
        }

    }
}

pub struct SDAccelFuncBuilder {
    pub ty: SDAccelFuncType,
    pub args:Vec<String>,
}

impl SDAccelFuncBuilder {

    pub fn add_param(&mut self, param: String) -> () {
        self.args.push(param);
    }
    pub fn add_params(&mut self, params: &Vec<String>) -> () {
        self.args.extend_from_slice(params);
    }

    pub fn gen_funcname(&mut self) -> String {
        format!("{}", self.ty)
    }

    pub fn emit (&mut self)-> String{
        let funcname = self.gen_funcname();
        let params_str = self.args.join(SDACCEL_ARG_SEPARATOR);
        format!("{}({})", funcname, params_str)
    }
}

pub fn sym_key_from_sdacceltype(ty: SDAccelType) -> String {
    match ty {
        SDAccelType::CLInt => SDACCEL_CL_INT_SYM_KEY.to_string(),
        SDAccelType::CLProgram => SDACCEL_CL_PROGRAM_SYM_KEY.to_string(),
        SDAccelType::CLKernel => SDACCEL_CL_KERNEL_SYM_KEY.to_string(),
        SDAccelType::CLMem => SDACCEL_CL_MEM_SYM_KEY.to_string(),
        SDAccelType::XCLWorld => SDACCEL_CL_WORLD_SYM_KEY.to_string(),
    }
}

pub struct SDAccelVar{
    pub sym: Symbol,
    pub ty: SDAccelType,
}
impl SDAccelVar{

    pub fn gen_typename(&mut self) -> String {
        format!("{}", self.ty)
    }

    pub fn gen_name(&mut self) -> String {
        format!("{}{}", &self.sym.name, &self.sym.id)
    }
    pub fn gen_var(&mut self) -> String {
        format!("{} {}", self.gen_typename(), self.gen_name())
    }
    pub fn gen_declare(&mut self) -> String {
        format!("{};", self.gen_var())
    }

    pub fn gen_declare_assign_str(&mut self, assi: &str) -> String {
        assign(self.gen_var(), assi.to_string())
    }

    pub fn gen_declare_assign(&mut self, assi: String) -> String {
        assign(self.gen_var(), assi)
    }
}
