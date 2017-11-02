use super::ast::*;
use super::sdaccel_util::*;
use std::fmt;

pub const SDACCEL_CL_INT_SYM_KEY: &'static str = "cl_int";
pub const SDACCEL_CL_PROGRAM_SYM_KEY: &'static str = "cl_program";
pub const SDACCEL_CL_KERNEL_SYM_KEY: &'static str = "cl_kernel";
pub const SDACCEL_CL_MEM_SYM_KEY: &'static str = "cl_mem";
pub const SDACCEL_CL_WORLD_SYM_KEY: &'static str = "cl_world";
pub const SDACCEL_CL_COMMMANDQ_SYM_KEY: &'static str = "cl_commandq";
pub const SDACCEL_CL_EVENT_SYM_KEY: &'static str = "cl_event";
pub const SDACCEL_CL_VECTOR_SYM_KEY: &'static str = "myvector";
#[derive(Clone)]
pub enum SDAccelType {
    CLInt,
    CLProgram,
    CLKernel,
    CLEvent,
    CLMem,
    XCLWorld,
    CLCommandQueue,
    Vector(Box<SDAccelType>, usize),
}

impl fmt::Display for SDAccelType{
    fn fmt(& self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone() {
            SDAccelType::CLInt => write!(f, "cl_int"),
            SDAccelType::CLProgram => write!(f, "cl_program"),
            SDAccelType::CLKernel => write!(f, "cl_kernel"),
            SDAccelType::CLMem => write!(f, "cl_mem"),
            SDAccelType::CLEvent => write!(f, "cl_event"),
            SDAccelType::XCLWorld => write!(f, "xcl_world"),
            SDAccelType::CLCommandQueue => write!(f, "cl_command_queue"),
            SDAccelType::Vector(ref mut x, _) => write!(f, "vector<{}>", x),
        }
    }
}

pub enum SDAccelFuncType {
    CLCreateBuffer,
    XCLImportBinary,
    XCLGetKernel,
    XCLSetKernelArg,
    CLCreateCommandQueue,
    Check,
    CLEnqueueNDRangeKernel,
    SetCallback,
    CLEnqueueReadBuffer,
    XCLMalloc,
    CLWaitForEvents,
    CLReleaseEvent,
    CLReleaseCommandQueue,
    CLReleaseMemObject,
    CLReleaseKernel,
    CLReleaseProgram,
    XCLReleaseWorld,
}

impl fmt::Display for SDAccelFuncType{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SDAccelFuncType::CLCreateBuffer => write!(f, "clCreateBuffer"),
            SDAccelFuncType::XCLImportBinary => write!(f, "xcl_import_binary"),
            SDAccelFuncType::XCLGetKernel => write!(f, "xcl_get_kernel"),
            SDAccelFuncType::XCLSetKernelArg => write!(f, "xcl_set_kernel_arg"),
            SDAccelFuncType::CLCreateCommandQueue => write!(f, "XCLSetKernelArg"),
            SDAccelFuncType::Check => write!(f, "check"),
            SDAccelFuncType::CLEnqueueNDRangeKernel => write!(f, "clEnqueueNDRangeKernel"),
            SDAccelFuncType::SetCallback => write!(f, "set_callback"),
            SDAccelFuncType::CLEnqueueReadBuffer => write!(f, "clEnqueueReadBuffer"),
            SDAccelFuncType::XCLMalloc => write!(f, "xcl_malloc"),
            SDAccelFuncType::CLWaitForEvents => write!(f, "clWaitForEvents"),
            SDAccelFuncType::CLReleaseEvent => write!(f, "clReleaseEvent"),
            SDAccelFuncType::CLReleaseCommandQueue => write!(f, "clReleaseCommandQueue"),
            SDAccelFuncType::CLReleaseMemObject => write!(f, "clReleaseMemObject"),
            SDAccelFuncType::CLReleaseKernel => write!(f, "clReleaseKernel"),
            SDAccelFuncType::CLReleaseProgram => write!(f, "clReleaseProgram"),
            SDAccelFuncType::XCLReleaseWorld => write!(f, "xcl_release_world"),

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
    pub fn emit_line (&mut self)-> String{
        format!("{};", self.emit())
    }

}

pub fn sym_key_from_sdacceltype(ty: SDAccelType) -> String {
    match ty {
        SDAccelType::CLInt => SDACCEL_CL_INT_SYM_KEY.to_string(),
        SDAccelType::CLProgram => SDACCEL_CL_PROGRAM_SYM_KEY.to_string(),
        SDAccelType::CLKernel => SDACCEL_CL_KERNEL_SYM_KEY.to_string(),
        SDAccelType::CLMem => SDACCEL_CL_MEM_SYM_KEY.to_string(),
        SDAccelType::XCLWorld => SDACCEL_CL_WORLD_SYM_KEY.to_string(),
        SDAccelType::CLCommandQueue => SDACCEL_CL_COMMMANDQ_SYM_KEY.to_string(),
        SDAccelType::CLEvent => SDACCEL_CL_EVENT_SYM_KEY.to_string(),
        SDAccelType::Vector(_,_) => SDACCEL_CL_VECTOR_SYM_KEY.to_string(),
    }
}

#[derive(Clone)]
pub struct SDAccelVar{
    pub sym: Symbol,
    pub ty: SDAccelType,
}
impl SDAccelVar{

    pub fn gen_typename(& self) -> String {
        format!("{}", self.ty)
    }

    pub fn gen_name(& self) -> String {
        format!("{}{}", &self.sym.name, &self.sym.id)
    }

    pub fn gen_name_sim(& self) -> String {
        format!("{}", &self.sym.name)
    }

    pub fn gen_ref(& self) -> String {
        format!("&{}", self.gen_name())
    }

    pub fn gen_ref_idx(& self, idx: usize) -> String {
        format!("&{}[{}]", self.gen_name(), idx)
    }


    pub fn gen_var(& self) -> String {
        format!("{} {}", self.gen_typename(), self.gen_name())
    }

    pub fn gen_var_attr(& self, child:&str) ->String {
        format!("{}.{}", self.gen_name(), child)

    }
    pub fn gen_declare(& self) -> String {
        match self.ty.clone() {
            SDAccelType::Vector(_, num) => format!("{}[{}];", &self.gen_var(), num),
            _ => format!("{};", self.gen_var())
        }
    }

    pub fn gen_declare_assign_str(& self, assi: &str) -> String {
        assign(self.gen_var(), assi.to_string())
    }

    pub fn gen_declare_assign(& self, assi: String) -> String {
        assign(self.gen_var(), assi)
    }
}
