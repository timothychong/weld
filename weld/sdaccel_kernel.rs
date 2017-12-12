
use std::fmt;

use std::collections::{BTreeMap, HashMap};
use super::ast::*;
use super::ast::LiteralKind::*;
use super::error::*;
use super::util::*;
use super::sdaccel_util::*;
use super::pretty_print::*;


#[derive(Clone, Debug)]
pub enum Attribute {
    AlwaysInline,
    PipelineLoop,
    UnrollHint
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Attribute::AlwaysInline => write!(f, "always_inline"),
            Attribute::PipelineLoop => write!(f, "xcl_pipeline_loop"),
            Attribute::UnrollHint => write!(f, "opencl_unroll_hint(UNROLL_FACTOR)"),
        }

    }
}



pub type BasicBlockId = usize;
pub type FunctionId = usize;
pub type BuffId = usize;

fn join<T: Iterator<Item = String>>(start: &str, sep: &str, end: &str, strings: T) -> String {
    let mut res = String::new();
    res.push_str(start);
    for (i, s) in strings.enumerate() {
        if i > 0 {
            res.push_str(sep);
        }
        res.push_str(&s);
    }
    res.push_str(end);
    res
}

pub fn print_type(ty: &Type) -> String {
    gen_scalar_type(ty).unwrap()
}

pub fn print_literal(lit: &LiteralKind) -> String {
    match *lit {
        BoolLiteral(v) => format!("{}", v),
        I8Literal(v) => format!("{}", v),
        I16Literal(v) => format!("{}", v),
        I32Literal(v) => format!("{}", v),
        I64Literal(v) => format!("{}", v),
        U8Literal(v) => format!("{}", v),
        U16Literal(v) => format!("{}", v),
        U32Literal(v) => format!("{}", v),
        U64Literal(v) => format!("{}", v),
        F32Literal(v) => {
            let mut res = format!("{}", f32::from_bits(v));
            // Hack to disambiguate from integers.
            if !res.contains(".") {
                res.push_str(".0f");
            }
            res.push_str("");
            res
        }
        F64Literal(v) => {
            let mut res = format!("{}", v);
            // Hack to disambiguate from integers.
            if !res.contains(".") {
                res.push_str(".0");
            }
            res
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    BinOp {
        output: Symbol,
        op: BinOpKind,
        left: Symbol,
        right: Symbol,
    },
    UnaryOp {
        output: Symbol,
        op: UnaryOpKind,
        child: Symbol,
    },
    Negate { output: Symbol, child: Symbol },
    Broadcast { output: Symbol, child: Symbol },
    Cast {
        output: Symbol,
        child: Symbol,
    },
    Lookup {
        output: Symbol,
        child: Symbol,
        index: Symbol,
    },
    KeyExists {
        output: Symbol,
        child: Symbol,
        key: Symbol,
    },
    Slice {
        output: Symbol,
        child: Symbol,
        index: Symbol,
        size: Symbol,
    },
    Select {
        output: Symbol,
        cond: Symbol,
        on_true: Symbol,
        on_false: Symbol,
    },
    CUDF {
        output: Symbol,
        symbol_name: String,
        args: Vec<Symbol>,
    },
    ToVec { output: Symbol, child: Symbol },
    Length { output: Symbol, child: Symbol },
    Assign { output: Symbol, value: Symbol },
    AssignLiteral { output: Symbol, value: LiteralKind },
    Merge { builder: Symbol, value: Symbol },
    Res { output: Symbol, builder: Symbol },
    NewBuilder {
        output: Symbol,
        arg: Option<Symbol>,
        ty: Type,
    },
    MakeStruct {
        output: Symbol,
        elems: Vec<Symbol>,
    },
    MakeVector {
        output: Symbol,
        elems: Vec<Symbol>,
    },
    GetField {
        output: Symbol,
        value: Symbol,
        index: u32,
    },
    // New ones
    Func {
        name: String,
        args: HashMap<Symbol, Type>,
        args_size: HashMap<Symbol, Type>,
        buffs: HashMap<Symbol, Type>,
        buffs_size: HashMap<Symbol, Type>,
    },

    For {
        init: Vec<Statement>,
        cond: Vec<Statement>,
        end: Vec<Statement>,
        inner: Vec<Statement>,
        attr: Vec<Attribute>,
    },

    BinOpNoAssign {
        op: BinOpKind,
        left: Symbol,
        right: Symbol,
    },


    BinOpLiteralRight {
        op: BinOpKind,
        left: Symbol,
        right: LiteralKind,
    },

    AssignStatement {
        output: Symbol,
        right: Box<Statement>
    },

    //InstantiateAssignReference {
        //left: Symbol,
        //ty: Type,
        //right: Symbol
    //},
    //
    GlobalInstantiateAssign{
        left: Symbol,
        ty: Type,
        right: Symbol
    },

    LocalInstantiateAssign{
        left: Symbol,
        ty: Type,
        right: Symbol
    },

    InstantiateAssign{
        left: Symbol,
        ty: Type,
        right: Symbol
    },

    Instantiate{
        left: Symbol,
        ty: Type,
    },

    InstantiateAssignIndexRight {
        left: Symbol,
        ty: Type,
        right: Symbol,
        index: Symbol,
    },

    IndexAssignLeft {
        output: Symbol,
        value: Symbol,
        index: Symbol,
    },

    //IndexAssignRight {
        //output: Symbol,
        //value: Symbol,
        //index: Symbol,
    //}

    If {
        cond: Symbol,
        true_blk: Vec<Statement>,
        false_blk: Vec<Statement>,
    },

    AsyncWorkGroupCopy {
        from: Symbol,
        from_offset: Symbol,
        to: Symbol,
        to_offset: Symbol,
        width: Symbol,
        ty: Type,
    },
}


impl Symbol {
    pub fn index (&self, i:String) -> Symbol{
        let mut s = self.clone();
        s.name.push_str(&format!("[{}]", i));
        s
    }
}

pub struct SDAccelKernel {

    pub funcs: Vec<SDAccelFunction>,
    pub sym_gen: SymbolGenerator,
    pub buffs: HashMap<Symbol, Type>,

    // This is for temporary storing things for code gen
    ret_sym: Option<Symbol>,
    ret_ty: Type,

    pub use_vector: bool,
}



impl SDAccelKernel {

    pub fn set_ret_sym(&mut self, sym: &Symbol) {
        self.ret_sym = Some(sym.clone());
        self.funcs[0].ret_sym = self.ret_sym.clone();
    }

    pub fn get_ret_sym(& self)-> Symbol{
        self.ret_sym.clone().unwrap()
    }

    pub fn new(ret_ty: &Type) -> SDAccelKernel {
        let mut prog = SDAccelKernel {
            funcs: vec![],
            sym_gen: SymbolGenerator::new(),
            buffs: HashMap::new(),
            ret_sym: None,
            ret_ty: ret_ty.clone(),
            use_vector: false
        };
        //add main
        prog.add_func();
        prog
    }

    pub fn add_func(&mut self) -> FunctionId {
        let mut func = SDAccelFunction {
            id: self.funcs.len(),
            params: HashMap::new(),
            params_size: HashMap::new(),
            blocks: vec![],
            locals: HashMap::new(),
            attr: vec![],
            buffs: HashMap::new(),
            buffs_size: HashMap::new(),
            local_buffs: HashMap::new(),
            // Global map per function
            all: HashMap::new(),
            ret_ty: None,
            ret_sym: None,

            variable_map: HashMap::new(),
        };

        if self.funcs.len() == 0 {
            func.ret_ty = Some(self.ret_ty.clone());
        }
        self.funcs.push(func);
        self.funcs.len() - 1
    }

    pub fn sym_from_buffid(&self, id: BuffId) -> Symbol {
        Symbol {
            name: format!("{}{}", SDACCEL_BUFFER_BUILD_PREFIX, id),
            id: 0
        }
    }

    pub fn add_global_buff(&mut self, ty: &Type, func_id: FunctionId)
        -> WeldResult<(Symbol)> {

        match *ty {
            Type::Vector(_) => {
                let id = self.buffs.len();
                let sym = self.sym_from_buffid(id);
                self.funcs[func_id].add_buff(&sym, ty);
                self.funcs[0].add_buff(&sym, ty);
                self.buffs.insert(sym.clone(), ty.clone());
                Ok((sym))
            }

            Type::Scalar(_) => {
                let s = self.new_sym(func_id);
                self.funcs[func_id].add_buff(&s, ty);
                self.funcs[0].add_local(&s, ty);
                Ok(s)
            }

            _ => weld_err!("Error: add_global_buff not a scalar type")

        }
    }

    /// Add a local variable of the given type and return a symbol for it.
    pub fn new_sym(&mut self, func:FunctionId) -> Symbol {
        let sym = self.sym_gen.new_symbol(format!("fn{}_tmp", func).as_str());
        sym
    }

    pub fn add_local_buff(&mut self, ty: &Type, func: FunctionId) -> WeldResult<Symbol> {
        let sym = self.new_sym(func);

        // right now assume all local buffs have the same size
        match *ty {
            Type::Vector(_) => {
                self.funcs[func].add_local_buff(&sym, &ty);
                Ok(sym)
            }

            Type::Builder(ref bk, _) => {
                match *bk {
                    BuilderKind::Appender(ref t) => {
                        let ty = Type::Vector(t.clone());
                        self.funcs[func].add_local_buff(&sym, &ty);
                        Ok(sym)

                    }
                    BuilderKind::Merger(_, _) => {
                        self.add_local(&ty, func);
                        Ok(sym)
                    }
                    _ =>return weld_err!("Local buff builder can only be appender/merger")
                }

            }
            _ => return weld_err!("Local buff can only be of type vector")
        }
    }

    pub fn add_local(&mut self, ty: &Type, func: FunctionId) -> Symbol {
        let sym = self.new_sym(func);
        self.add_local_named(ty, &sym, func);
        sym
    }

    /// Add a local variable of the given type and name
    pub fn add_local_named(&mut self, ty: &Type, sym: &Symbol, func: FunctionId) {
        match *ty {
            Type::Struct(ref ts) => {
                for (i, ty) in ts.iter().enumerate() {
                    let sym = struct_sym(&sym, i);
                    self.funcs[func].add_local(&sym, &ty);
                }
            }
            _ => {
                self.funcs[func].add_local(&sym, &ty);
            }
        }
    }

    pub fn update_all_func_size_param(&mut self) {
        for func in &mut self.funcs {
            func.update_vec_with_size();
        }
    }


    pub fn assignments(&self, output:&Symbol, value: &Symbol, ty: &Type,
              vec: &mut Vec<Statement> ) {
        use self::Statement::*;

        match *ty {
            Type::Struct(ref ts) => {
                for (i, _) in ts.iter().enumerate() {
                    let output = struct_sym(&output, i);
                    let value = struct_sym(&value, i);
                    vec.push(
                    Assign {
                            output: output.clone(),
                            value: value.clone(),
                    });
                }
            }
            Type::Builder(ref bk, _) => {
                match *bk {
                    BuilderKind::Merger(_, _) => {
                        vec.push(
                        Assign {
                            output: output.clone(),
                            value: value.clone(),
                            });
                        //Assign {
                            //output: deref_sym(&output),
                            //value: deref_sym(&value),
                            //});
                    }
                    _ => {
                            vec.push(
                            Assign {
                                output: output.clone(),
                                value: value.clone(),
                                });
                    }
                }

            }
            _ => {
                    vec.push(
                    Assign {
                        output: output.clone(),
                        value: value.clone(),
                        });
            }

        }


    }

    pub fn assign(&mut self, output:&Symbol, value: &Symbol, ty: &Type,
              cur_func:FunctionId, cur_block: BasicBlockId ) {

        let mut v = Vec::new();
        self.assignments(output, value, ty, &mut v);
        for s in v {
            self.funcs[cur_func].blocks[cur_block].add_statement(
                s.clone()
            );
        }
    }

    pub fn assign_var_map(&mut self, target: &Symbol, obj: &Symbol,
        func_id: FunctionId) {
        self.funcs[func_id].assign_var_map(target, obj);
    }

    pub fn find_var_target(& self, target: &Symbol) -> Symbol {
        self.funcs[0].find_var_target(target)
    }

}

pub fn extra_vec_with_size(hm: &HashMap<Symbol, Type>) -> HashMap<Symbol, Type> {

    let mut extra = HashMap::new();
    for (sym, ty) in hm {
        match *ty {
            Type::Vector(_) => {
                if !sym.name.ends_with(SDACCEL_SIZE_SUFFIX) {
                    let mut s = sym.clone();
                    s.name = gen_name_size(&s.name);
                    extra.insert(s, SDACCEL_SIZE_TYPE);
                }
            }
            _ => {}
        }
    }
    extra
}

pub struct SDAccelFunction {
    pub id: FunctionId,
    pub blocks: Vec<BasicBlock>,
    pub attr: Vec<Attribute>,
    locals: HashMap<Symbol, Type>,
    params: HashMap<Symbol, Type>,
    buffs: HashMap<Symbol, Type>,
    buffs_size: HashMap<Symbol, Type>,
    params_size: HashMap<Symbol, Type>,
    pub ret_ty: Option<Type>,
    pub ret_sym: Option<Symbol>,
    pub all: HashMap<Symbol, Type>,
    local_buffs: HashMap<Symbol, Type>,

    pub variable_map: HashMap<Symbol, Symbol>,
}

impl SDAccelFunction {

    pub fn add_block(&mut self) -> BasicBlockId {
        self.add_block_full(false)
    }
    // Inner block is not printed out like other blocks. It's the inner block for
    // for loops
    pub fn add_inner_block(&mut self) -> BasicBlockId {
        self.add_block_full(true)
    }


    pub fn add_block_full(&mut self, is_inner_block:bool) -> BasicBlockId {
        let block = BasicBlock {
            id: self.blocks.len(),
            statements: vec![],
            is_inner_block: is_inner_block,
        };
        self.blocks.push(block);
        self.blocks.len() - 1
    }
    pub fn add_buff(&mut self, sym: &Symbol, ty: &Type) {
        self.buffs.insert(sym.clone(), ty.clone());
        self.all.insert(sym.clone(), ty.clone());
    }

    pub fn add_local_buff(&mut self, sym: &Symbol, ty: &Type) {
        self.local_buffs.insert(sym.clone(), ty.clone());
        self.all.insert(sym.clone(), ty.clone());
    }

    pub fn add_param(&mut self, sym: &Symbol, ty: &Type) {
        self.params.insert(sym.clone(), ty.clone());
        self.all.insert(sym.clone(), ty.clone());
    }

    pub fn add_local(&mut self, sym: &Symbol, ty: &Type) {
        self.locals.insert(sym.clone(), ty.clone());
        self.all.insert(sym.clone(), ty.clone());
    }

    pub fn func_name(& self) -> String {
        format!("func_{}", self.id)
    }

    pub fn assign_var_map(&mut self, target: &Symbol, obj: &Symbol) {
        self.variable_map.insert(obj.clone(), target.clone());
    }

    pub fn find_var_target(& self, target: &Symbol) -> Symbol {
        let mut result = target.clone();

        while self.variable_map.contains_key(&result) {
            if let Some(x) = self.variable_map.get(&result) {
                result = x.clone();
            }
        }
        //assert!(result != *target);
        return result;
    }


    pub fn update_vec_with_size(&mut self) {
        let extra = extra_vec_with_size(&mut self.params);
        self.params_size.extend(extra);

        let extra = extra_vec_with_size(&mut self.buffs);
        self.buffs_size.extend(extra);

        #[allow(unused_variables)]
        for block in &mut self.blocks {
            for s in &mut block.statements{
                if let Statement::Func {
                    ref name,
                    ref args,
                    ref args_size,
                    ref buffs,
                    ref buffs_size
                } = s.clone() {
                    let new_args_size = extra_vec_with_size(args);
                    let new_buffs_size = extra_vec_with_size(buffs);
                    // Processing buff
                    *s = Statement::Func {
                        args: args.clone(),
                        buffs: buffs.clone(),
                        name: name.clone(),
                        args_size: new_args_size,
                        buffs_size: new_buffs_size,
                    };
                }
            }
        }

    }

    //pub fn find_sym_type(& self, sym: &Symbol) -> WeldResult<Type>{
        //if let Some(x) = self.all.get(&sym) {
            //return Ok(x.clone())
        //}
        //weld_err!("Find_Sym_Type not found: {:?}", sym)
    //}

    pub fn add_func_counter(&mut self, sym: &Symbol) {
        if !self.locals.contains_key(sym) {
            self.locals.insert(sym.clone(), SDACCEL_COUNTER_TYPE);
        }
    }
}

#[derive(Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub statements: Vec<Statement>,
    pub is_inner_block:bool,
}

impl BasicBlock {
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

impl fmt::Display for Statement {
    #[allow(unused_variables)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match *self {
            BinOp {
                ref output,
                ref op,
                ref left,
                ref right,
            } => {
                write!(f,
                       "{} = {} {} {}",
                       output,
                       left,
                       op,
                       right)
            }
            UnaryOp {
                ref output,
                ref op,
                ref child,
            } => write!(f, "{} = {}({})", output, op, child),
            Negate {
                ref output,
                ref child,
            } => write!(f, "{} = -{}", output, child),
            Broadcast {
                ref output,
                ref child,
            } => write!(f, "{} = broadcast({})", output, child),
            Cast {
                ref output,
                ref child,
            } => write!(f, "{} = cast({})", output, child),
            Lookup {
                ref output,
                ref child,
                ref index,
            } => write!(f, "{} = lookup({}, {})", output, child, index),
            KeyExists {
                ref output,
                ref child,
                ref key,
            } => write!(f, "{} = keyexists({}, {})", output, child, key),
            Slice {
                ref output,
                ref child,
                ref index,
                ref size,
            } => write!(f, "{} = slice({}, {}, {})", output, child, index, size),
            Select {
                ref output,
                ref cond,
                ref on_true,
                ref on_false,
            } => write!(f, "{} = select({}, {}, {})", output, cond, on_true, on_false),
            ToVec {
                ref output,
                ref child,
            } => write!(f, "{} = toVec({})", output, child),
            Length {
                ref output,
                ref child,
                ..
            } => write!(f, "{} = len({})", output, child),
            Assign {
                ref output,
                ref value,
            } => write!(f, "{} = {}", output, value),
            AssignLiteral {
                ref output,
                ref value,
            } => {
                write!(f, "{} = {}", output, print_literal(value))
            }
            Merge {
                ref builder,
                ref value,
            } => write!(f, "merge({}, {})", builder, value),
            Res {
                ref output,
                ref builder,
            } => write!(f, "{} = result({})", output, builder),
            NewBuilder {
                ref output,
                ref arg,
                ref ty,
            } => {
                let arg_str = if let Some(ref a) = *arg {
                    a.to_string()
                } else {
                    "".to_string()
                };
                //write!(f, "{} = new {}({})", output, print_type(ty), arg_str)
                write!(f, "{} = new SOMETYPE({})", output , arg_str)
            }
            MakeStruct {
                ref output,
                ref elems,
            } => {
                write!(f,
                       "{} = {}",
                       output,
                       join("{", ",", "}", elems.iter().map(|e| format!("{}", e))))
            }
            MakeVector {
                ref output,
                ref elems,
            } => {
                write!(f,
                       "{} = {}",
                       output,
                       join("[", ", ", "]", elems.iter().map(|e| format!("{}", e))))
            }
            CUDF {
                ref output,
                ref args,
                ref symbol_name,
                ..
            } => {
                write!(f,
                       "{} = cudf[{}]{}",
                       output,
                       symbol_name,
                       join("(", ", ", ")", args.iter().map(|e| format!("{}", e))))
            }
            GetField {
                ref output,
                ref value,
                index,
            } => write!(f, "{} = {}.${}", output, value, index),

            Func {
                ref name,
                ref args,
                ref args_size,
                ref buffs,
                ref buffs_size,
            } => {
                let sorted: BTreeMap<&Symbol, &Type> = args.iter().collect();
                let sorted_buffs: BTreeMap<&Symbol, &Type> = buffs.iter().collect();
                let sorted_buffs_size: BTreeMap<&Symbol, &Type> = buffs_size.iter().collect();
                let sorted_args_size: BTreeMap<&Symbol, &Type> = args_size.iter().collect();
                write!(f, "{}{}",
                           name,
                           join("(", ", ", "", sorted.iter().map(|e| format!("{}", e.0))))?;
                if sorted_args_size.len() > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", join("", ", ", "", sorted_args_size.iter().map(|e| format!("&{}", e.0))))?;

               fn get_cl_arg (ty: &Type, sym: &Symbol) -> String {
                   match *ty {
                    Type::Scalar(_) => format!("&{}", sym),
                       _ => format!("{}", sym),
                   }
               }
                if sorted_buffs.len() > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", join("", ", ", "",
                                     sorted_buffs.iter().map(
                                         |e| format!("{}", get_cl_arg(&e.1, &e.0)))))?;

                if sorted_buffs_size.len() > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", join("", ", ", "", sorted_buffs_size.iter().map(|e| format!("&{}", e.0))))?;
                write!(f, ")")

            }

            For {
                ref init,
                ref cond,
                ref end,
                ref inner,
                ref attr,
            } => {
                if attr.len() > 0 {
                    for i in attr {
                        write!(f, "__attribute__(({}))\n", i)?;
                    }
                }
                write!(f, "  for({}; {}; {}){{\n{}\n  }}",
                    join("", ", ", "", init.iter().map(|e| format!("{}", e))),
                    join("", " && ", "", cond.iter().map(|e| format!("{}", e))),
                    join("", ", ", "", end.iter().map(|e| format!("{}", e))),
                    join("    ", ";\n    ", ";\n", inner.iter().map(|e| format!("\t{}", e))),
                )

            }

            BinOpNoAssign {
                ref op,
                ref left,
                ref right,
            } => {
                write!(f,
                       "{} {} {}",
                       left,
                       op,
                       right)
            }

            BinOpLiteralRight {
                ref op,
                ref left,
                ref right,
            } => {
                write!(f,
                       "{} {} {}",
                       left,
                       op,
                       print_literal(right))
            }

            AssignStatement {
                ref output,
                ref right,
            } => {
                write!(f,
                       "{} = {}",
                       output,
                       *right)
            }

            //InstantiateAssignReference {
                //ref left,
                //ref ty,
                //ref right
            //} => {
                //write!(f, "{}& {} = {}", print_type(ty), left, right)
            //}
            //
            Instantiate {
                ref left,
                ref ty,
            } => {
                write!(f, "{} {}", print_type(ty), left)
            }

            InstantiateAssign {
                ref left,
                ref ty,
                ref right
            } => {
                write!(f, "{} {} = {}", print_type(ty), left, right)
            }

            GlobalInstantiateAssign {
                ref left,
                ref ty,
                ref right
            } => {
                write!(f, "__global {} {} = {}", print_type(ty), left, right)
            }

            LocalInstantiateAssign {
                ref left,
                ref ty,
                ref right
            } => {
                write!(f, "__local {} {} = {}", print_type(ty), left, right)
            }

            InstantiateAssignIndexRight {
                ref left,
                ref ty,
                ref right,
                ref index,
            } => {
                write!(f, "{} {} = {}[{}]", print_type(ty), left, right, index)
            }

            IndexAssignLeft {
                ref output,
                ref index,
                ref value
            } => {
                write!(f, "{}[{}] = {}", output, index, value)
            }

            If {
                ref cond,
                ref true_blk,
                ref false_blk,
            } => {
                write!(f, "if({})", cond)?;
                write!(f, "{{\n")?;
                if true_blk.len() != 0 {
                    write!(f, "{}", join("", ";\n", ";\n",
                                     true_blk.iter().map(|e| format!("\t{}", e))))?;
                }
                write!(f, "    }} else {{\n    ")?;
                if false_blk.len() != 0{
                    write!(f, "  {}", join("", ";\n", ";\n",
                                   false_blk.iter().map(|e| format!("\t{}", e))))?;
                }
                write!(f, "  }}")
            }

            AsyncWorkGroupCopy {
                ref from,
                ref from_offset,
                ref to,
                ref to_offset,
                ref width,
                ref ty,
            } => {
                if let Type::Vector(ref t) = *ty {
                    write!(f, "async_work_group_copy({} + {}, {} + {}, LOCAL_BUFF_SIZE, 0)",
                               from, from_offset, to, to_offset)
                } else if let Type::Builder(ref bk, _) = *ty {
                    match *bk {
                        BuilderKind::Appender(ref t) => {
                            write!(f, "async_work_group_copy({} + {}, {} + {}, LOCAL_BUFF_SIZE, 0)",
                               from, from_offset, to, to_offset)
                        } ,
                        _ => write!(f, " async work builder must be appender")
                    }
                } else {
                    write!(f,"async work group copy not vector: {:?}", ty)
                }
            }


        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //write!(f, "B{}:\n", self.id)?;
        for stmt in &self.statements {
            write!(f, "  {};\n", stmt)?;
        }
        Ok(())
    }
}

impl fmt::Display for SDAccelFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.id == 0 {
            write!(f, "__kernel ")?;
        }

       let pointer = if self.id == 0 {""} else {"*"};
       for attr in &self.attr {
            write!(f, "__attribute__ (({}))\n", attr)?;
        }
       write!(f,"void {}", self.func_name())?;

       fn get_cl_type_param (ty: &Type) -> String {
           match *ty {
            Type::Vector(_) => format!("__global {}", gen_scalar_type(ty).unwrap()),
            Type::Scalar(_) => format!("{} ", gen_scalar_type(ty).unwrap()),
               _ => String::from("")
           }

       }
       //Params here
        let params_sorted: BTreeMap<&Symbol, &Type> = self.params.iter().collect();
       write!(f, "{}", join("(", ", ", "",
               params_sorted.iter().map(
                   |e| format!("{} {}", get_cl_type_param(e.1), e.0)
                   )))?;

        let paramsize_sorted: BTreeMap<&Symbol, &Type> = self.params_size.iter().collect();
       if paramsize_sorted.len() > 0 {
           write!(f, ", ")?;
       }
       write!(f, "{}", join("", ", ", "",
               paramsize_sorted.iter().map(
                   |e| format!("{} {} {}", gen_scalar_type(e.1).unwrap(), pointer, e.0)
                   )))?;

       let buffs_sorted: BTreeMap<&Symbol, &Type> = self.buffs.iter().collect();
       if buffs_sorted.len() > 0 {
           write!(f, ", ")?;
       }

       fn get_cl_type (ty: &Type) -> String {
           match *ty {
            Type::Vector(_) => format!("__global {}", gen_scalar_type(ty).unwrap()),
            Type::Scalar(_) => format!("{} *", gen_scalar_type(ty).unwrap()),
               _ => String::from("")
           }
       }

       write!(f, "{}", join("", ", ", "",
               buffs_sorted.iter().map(
                   |e| format!("{} {}", get_cl_type(e.1), e.0)
                   )))?;

       if self.id > 0 {
           let buffsize_sorted: BTreeMap<&Symbol, &Type> = self.buffs_size.iter().collect();
           if buffsize_sorted.len() > 0 {
               write!(f, ", ")?;
           }
           write!(f, "{}", join("", ", ", "",
                   buffsize_sorted.iter().map(
                       |e| format!("{} {} {}", gen_scalar_type(e.1).unwrap(), pointer, e.0)
                       )))?;
       }



       //for results
       if self.id == 0 {
           write!(f, ", ")?;
           if let Some(ref ret_ty) = self.ret_ty {
                match ret_ty {
                    &Type::Function(_, ref rt ) => {
                        match **rt {
                            Type::Vector(_) => {
                                write!(f, "__global {} * {}",
                                       gen_scalar_type_from_kind(&SDACCEL_SIZE_KIND),
                                       gen_name_result_mem(0))?;
                            }

                            Type::Scalar(ref k) => {
                                write!(f, "__global {} * {}",
                                       gen_scalar_type_from_kind(k),
                                       gen_name_result_mem(0))?;
                            }
                            _ => panic!("Only Return type vector + scalar supported")
                        }
                    }

                    _ => panic!("Only Return type vector + scalar supported")
                }

           }

       }

        write!(f, "){{\n\n")?;

       if self.id == 0 {
           let buffsize_sorted: BTreeMap<&Symbol, &Type> = self.buffs_size.iter().collect();
            for (name, ty) in buffsize_sorted {
                write!(f, "  {} {};\n", print_type(ty), name)?;
            }
       }

        let locals_sorted: BTreeMap<&Symbol, &Type> = self.locals.iter().collect();
        for (name, ty) in locals_sorted {
            if self.params.contains_key(&self.find_var_target(name)) ||
                self.buffs.contains_key(&self.find_var_target(name)) {
                match *ty {
                    Type::Vector(_) => {
                        write!(f, "__global {} {};\n", print_type(ty), name)?;
                    }
                    _ => {
                write!(f, "{} {};\n", print_type(ty), name)?;
                    }
                }
            } else {
                write!(f, "{} {};\n", print_type(ty), name)?;
            }
        }

        let local_buffs_sorted: BTreeMap<&Symbol, &Type> = self.local_buffs.iter().collect();
        for (name, ty) in local_buffs_sorted {
            if let Type::Vector(ref t) = *ty {
                write!(f, "local {} {}[{}];\n", print_type(t), name,
                SDACCEL_LOCAL_BUFF_SIZE_DEF
                )?;
            } else if let Type::Builder(ref bk, _) = *ty {
                match *bk {
                    BuilderKind::Appender(ref t) => {
                        write!(f, "local {} {}[{}];\n",
                               print_type(&t.as_ref().clone()), name,
                               SDACCEL_LOCAL_BUFF_SIZE_DEF
                               )?;
                    }

                    _ => {}
                }

            } else {
                panic!("Local buffs can only be vectors")
            }
        }

        for block in &self.blocks {
            if !block.is_inner_block {
                write!(f, "{}", block)?;
            }
        }

       if self.id == 0 {
           if let Some(ref ret_ty) = self.ret_ty {
                match ret_ty {
                    &Type::Function(_, ref rt ) => {
                        match **rt {
                            Type::Vector(_) => {
                                write!(f, "*{} = {};",
                                       gen_name_result_mem(0),
                                       gen_sym_size_sym( &self.ret_sym.clone().unwrap())
                                       )?;
                            }
                            Type::Scalar(_) => {
                                write!(f, "*{} = {};",
                                       gen_name_result_mem(0),
                                       &self.ret_sym.clone().unwrap()
                                       )?;
                            }
                            _ => panic!("Only Return type vector + scalar supported")
                        }
                    }
                    _ => panic!("Only Return type vector + scalar supported")
                }

           }

       }
        write!(f, "\n}}")?;
        Ok(())
    }
}

impl fmt::Display for SDAccelKernel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#define {} {}\n\n", SDACCEL_LOCAL_BUFF_SIZE_DEF, 512)?;
        write!(f, "#define UNROLL_FACTOR {}\n\n" , 16)?;
        for func in &self.funcs {
            if func.id != 0  {
                write!(f, "{}\n\n", func)?;
            }

        }
        write!(f, "{}\n\n", self.funcs[0])?;


        Ok(())
    }
}


/// Generate code to compute the expression `expr`
pub fn gen_expr(expr: &TypedExpr,
                prog: &mut SDAccelKernel,
                cur_func: FunctionId,
                cur_block: BasicBlockId,
                ) -> WeldResult<Symbol> {

    use self::Statement::*;
    match expr.kind {

        ExprKind::Ident(ref sym) => {
            if let None = prog.funcs[cur_func].all.get(sym) {
                match &prog.funcs[0].all.clone().get(sym) {
                    &Some(ref ty) => {
                        prog.funcs[cur_func].add_param(sym, &ty);
                    }
                    &None => {}
                }
            }


            Ok(sym.clone())
        },

        ExprKind::Literal(lit) => {
            let res_sym = prog.add_local(&expr.ty, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(AssignLiteral {
                                                                     output: res_sym.clone(),
                                                                     value: lit,
                                                                 });
            Ok(res_sym)
        }

        ExprKind::Let {
            ref name,
            ref value,
            ref body,
        } => {
            let val = gen_expr(value, prog, cur_func, cur_block)?;

            prog.add_local_named(&value.ty, name, cur_func);

            prog.assign(&name, &val, &value.ty, cur_func, cur_block);

            prog.funcs[cur_func].assign_var_map(
                &val,
                &name,
            );

            if let Type::Vector(_) = value.ty {
                //Create alias for vector size
                    let name = gen_sym_size_sym(name);
                    let val = gen_sym_size_sym(&val);
                    prog.add_local_named(&SDACCEL_SIZE_TYPE, &name, cur_func);

                    prog.funcs[cur_func].blocks[cur_block].add_statement(Assign {
                                                                         output: name.clone(),
                                                                         value: val.clone(),
                    });
            };

            let body_sym = gen_expr(body, prog, cur_func, cur_block)?;
            Ok(body_sym)
        }

        ExprKind::BinOp {
            kind,
            ref left,
            ref right,
        } => {
            let left_sym = gen_expr(left, prog, cur_func, cur_block)?;
            let right_sym = gen_expr(right, prog, cur_func, cur_block)?;
            let res_sym = prog.add_local(&expr.ty, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(BinOp {
                                                                     output: res_sym.clone(),
                                                                     op: kind,
                                                                     left: left_sym,
                                                                     right: right_sym,
                                                                 });
            Ok(res_sym)
        }

        ExprKind::UnaryOp {
            kind,
            ref value,
        } => {
            let value_sym = gen_expr(value, prog, cur_func, cur_block)?;
            let res_sym = prog.add_local(&expr.ty, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(UnaryOp {
                output: res_sym.clone(),
                op: kind,
                child: value_sym,
            });
            Ok(res_sym)
        }

        //ExprKind::Negate(ref child_expr) => {
            //println!("Negate")
        //}

        ExprKind::Broadcast(ref child_expr) => {
            let child_sym = gen_expr(child_expr, prog, cur_func, cur_block)?;
            let res_sym = prog.add_local(&expr.ty, cur_func);

            prog.assign(&res_sym, &child_sym, &expr.ty, cur_func, cur_block);

            Ok(res_sym)
        }

        ExprKind::Cast { ref child_expr, .. } => {
            let child_sym = gen_expr(child_expr, prog, cur_func, cur_block)?;
            let res_sym = prog.add_local(&expr.ty, cur_func);

            prog.assign(&res_sym, &child_sym, &expr.ty, cur_func, cur_block);

            Ok(res_sym)
        }

        //ExprKind::Lookup {
            //ref data,
            //ref index,
        //} => {
            //println!("data: {:?}", data);
            //println!("index: {:?}", index);
            //Ok(None)
        //}

        //ExprKind::KeyExists { ref data, ref key } => {
            //println!("KeyExists")
        //}

        //ExprKind::Slice {
            //ref data,
            //ref index,
            //ref size,
        //} => {
            //println!("Slice")
        //}

        //ExprKind::Select {
            //ref cond,
            //ref on_true,
            //ref on_false,
        //} => {
            //println!("Select")
        //}
        //ExprKind::ToVec { ref child_expr } => {
            //println!("ToVec")
        //}

        ExprKind::Length { ref data } => {
            // Need to look for length that is what we want
            let data_sym = gen_expr(data, prog, cur_func, cur_block)?;

            Ok(data_sym)
        }

        ExprKind::If {
            ref cond,
            ref on_true,
            ref on_false,
        } => {
            let cond_sym = gen_expr(cond, prog, cur_func, cur_block)?;

            let true_block = prog.funcs[cur_func].add_inner_block();
            let false_block = prog.funcs[cur_func].add_inner_block();

            let on_true_sym = gen_expr(on_true, prog, cur_func, true_block)?;
            let on_false_sym = gen_expr(on_false, prog, cur_func, false_block)?;

            let res_sym = prog.add_local(&expr.ty, cur_func);

            let mut true_stats = Vec::new();
            let mut false_stats = Vec::new();

            for s in &prog.funcs[cur_func].blocks[true_block].statements {
                true_stats.push(s.clone());
            }


            for s in &prog.funcs[cur_func].blocks[false_block].statements {
                false_stats.push(s.clone());
            }

            prog.assignments(&res_sym, &on_true_sym, &expr.ty, &mut true_stats);
            prog.assignments(&res_sym, &on_false_sym, &expr.ty, &mut false_stats);


            prog.funcs[cur_func].blocks[cur_block].add_statement(
                If {
                    cond: cond_sym,
                    true_blk: true_stats.clone(),
                    false_blk: false_stats.clone(),
                }
            );
            Ok(res_sym)
        }

        //ExprKind::Iterate {
            //ref initial,
            //ref update_func,
        //} => {
            //println!("Iterate")
        //}

        ExprKind::Merge {
            ref builder,
            ref value,
        } => {

            let builder_sym = gen_expr(builder, prog, cur_func, cur_block)?;
            let value_sym = gen_expr(value, prog, cur_func, cur_block)?;

            if let Type::Builder(ref bk, _) = builder.as_ref().ty {
                match *bk {
                    BuilderKind::Merger(_, ref op) => {
                        let post_builder_sym;
                        if prog.use_vector {
                            post_builder_sym = gen_sym_inner_sym(&builder_sym);
                        } else {
                            post_builder_sym = deref_sym(&builder_sym);
                        }
                        prog.funcs[cur_func].blocks[cur_block].add_statement(BinOp {
                                                     output: post_builder_sym.clone(),
                                                     op: *op,
                                                     left: post_builder_sym.clone(),
                                                     right: value_sym,
                                                 });

                    }
                    BuilderKind::Appender(_) => {
                        let post_counter_sym;

                        if prog.use_vector {
                            post_counter_sym = gen_sym_inner_size_sym(&builder_sym);
                        } else {
                            post_counter_sym = deref_sym(&gen_sym_size_sym(&builder_sym));
                        }

                        prog.funcs[cur_func].blocks[cur_block].add_statement(
                             IndexAssignLeft{
                                 output: builder_sym.clone(),
                                 index: post_counter_sym.clone(),
                                 value: value_sym.clone()
                             }
                        );
                        prog.funcs[cur_func].blocks[cur_block].add_statement(
                             AssignStatement {
                                 output: post_counter_sym.clone(),
                                 right: Box::new(BinOpLiteralRight {
                                     left: post_counter_sym.clone(),
                                     op: BinOpKind::Add,
                                     right: LiteralKind::U32Literal(1),
                                 })
                             }
                        );
                    }
                    _ => return weld_err!("Builder type not supported inside Merge")
                }
            };

            Ok(builder_sym.clone())
        }

        ExprKind::Res { ref builder } => {
            //let res_sym = prog.add_local(&expr.ty, cur_func);
            let builder_sym = gen_expr(builder, prog, cur_func, cur_block)?;
            //println!("Res builder kind: {}", print_expr(expr));
            Ok(builder_sym)
        }

        ExprKind::NewBuilder(ref arg) => {
            match *arg {
                Some(ref e) => {

                    if let ExprKind::Length{ref data} = e.kind {
                        if let Type::Vector(_) = data.ty{
                            let nbuilder = prog.add_global_buff(&(data.ty), cur_func)?;

                            //For now always create a new buffer
                            Ok(nbuilder)

                        } else {
                            return weld_err! ("Length can only be of a vector");
                        }

                    } else {
                        return weld_err! ("Only Length supported for\
                                          New builder inner type");
                    }
                }
                None => {
                    if let Type::Builder(ref bk, _) = expr.ty {
                        match *bk {
                            BuilderKind::Appender(ref t) => {
                                let ty = Type::Vector(t.clone());
                                let nbuilder = prog.add_global_buff(&ty, cur_func)?;
                                return Ok(nbuilder)
                            }

                            BuilderKind::Merger(ref t, _) => {
                                let nbuilder = prog.add_global_buff(t.as_ref(),
                                                    cur_func)?;
                                return Ok(nbuilder)
                            }

                            _ =>
                                return weld_err!("Builder type not supported: {}",
                                                 print_expr(expr))
                        }
                    } else {
                        return weld_err!("New Builder Type not a builder")
                    }
                }

            }
        }

        ExprKind::MakeStruct { ref elems } => {
            let es_sym = prog.add_local(&expr.ty, cur_func);
            for (i, e) in elems.iter().enumerate() {
                let e_sym = gen_expr(e, prog, cur_func, cur_block)?;
                //let e_sym_i = struct_sym(&e_sym, i);

                prog.funcs[cur_func].blocks[cur_block].add_statement(
                    Assign {
                        output: struct_sym(&es_sym, i).clone(),
                        value: e_sym.clone(),
                });

            }
            Ok(es_sym)
        }

        //ExprKind::MakeVector { ref elems } => {
            //println!("Make Vector")
        //}

        //ExprKind::CUDF {
            //ref sym_name,
            //ref args,
            //..
        //} => {
            //println!("CUDF")
        //}

        ExprKind::GetField { ref expr, index } => {
            let res_sym = gen_expr(expr, prog, cur_func, cur_block)?;
            // Combining id ito field
            let sym = struct_sym(&res_sym, index as usize);
            Ok(sym)

        }

        ExprKind::For {
            ref iters,
            ref builder,
            ref func,
        } => {
            if let IterKind::FringeIter = iters[0].kind {
                return gen_expr(builder, prog, cur_func, cur_block)
            }

            if let ExprKind::Lambda {
                       ref params,
                       ref body,
                   } = func.kind {

                let body_func = prog.add_func();
                let body_block = prog.funcs[body_func].add_block();
                prog.funcs[body_func].attr.push(Attribute::AlwaysInline);

                //The innter parts of the for lop
                let mut init:Vec<Statement> = Vec::new();
                let mut cond:Vec<Statement> = Vec::new();
                let mut end:Vec<Statement> = Vec::new();
                let mut inner:Vec<Statement> = Vec::new();

                let builder_sym = gen_expr(builder, prog, body_func, body_block)?;

                let lambda_builder = params[0].clone();
                let counter_sym = gen_sym_size_sym(&lambda_builder.name);
                let lambda_var = params[2].clone();

                if let Type::Builder(ref bk, _) = builder.as_ref().ty {
                    match *bk {
                        BuilderKind::Appender(_) => {
                            //Initialize builder reference
                            prog.funcs[body_func].blocks[body_block]
                                .add_statement(
                                // Setting builder reference
                                GlobalInstantiateAssign {
                                    left: lambda_builder.name.clone(),
                                    ty: builder.as_ref().ty.clone(),
                                    right: builder_sym.clone(),
                                }
                            );
                            prog.funcs[body_func].blocks[body_block]
                                .add_statement(
                                // Setting builder reference
                                InstantiateAssign {
                                    left: deref_sym(&counter_sym),
                                    ty: SDACCEL_SIZE_TYPE.clone(),
                                    right: gen_sym_size_sym(&builder_sym),
                                }
                            );
                            prog.funcs[body_func].blocks[body_block]
                                .add_statement(
                                    AssignLiteral {
                                        output: deref_sym(&counter_sym),
                                        value: LiteralKind::U32Literal(0),
                                    }
                            );
                        }

                        BuilderKind::Merger(ref ty, _) => {
                            prog.funcs[body_func].blocks[body_block]
                                .add_statement(
                                InstantiateAssign {
                                    left: deref_sym(&lambda_builder.name),
                                    ty: ty.as_ref().clone(),
                                    right: builder_sym.clone(),
                                }
                            );
                            prog.funcs[body_func].blocks[body_block]
                                .add_statement(
                                    AssignLiteral {
                                        output: deref_sym(&lambda_builder.name),
                                        value: LiteralKind::U32Literal(0),
                                    }
                            );
                        }
                        _ => return weld_err!("Builder type not supported \
                                              inside For")
                    }
                }

                let iter_kind = iters[0].kind.clone();


                let mut var_syms:Vec<Symbol> = Vec::new();
                let counter_sym = prog.add_local(&SDACCEL_COUNTER_TYPE, body_func);

                for (i, iter) in iters.iter().enumerate() {
                    let data = gen_expr(&iter.data, prog, body_func, body_block)?;
                    var_syms.push(data.clone());

                    prog.funcs[body_func].params.insert(data.clone(),
                                                iter.data.ty.clone());
                    if i == 0 {
                        prog.funcs[0].assign_var_map(
                            &data,
                            &builder_sym,
                        );
                        init.push(
                            AssignLiteral {
                                output: counter_sym.clone(),
                                value: LiteralKind::U32Literal(0),
                            }
                        );
                        cond.push(
                            BinOpNoAssign {
                                left: counter_sym.clone(),
                                right: deref_sym(&gen_sym_size_sym(&data)),
                                op: BinOpKind::LessThan,
                            }
                        );
                        match iter_kind {
                            IterKind::ScalarIter => {
                                end.push(
                                    AssignStatement {
                                        output: counter_sym.clone(),
                                        right: Box::new(BinOpLiteralRight {
                                            left: counter_sym.clone(),
                                            op: BinOpKind::Add,
                                            right: LiteralKind::U32Literal(1),
                                        })
                                    }
                                );
                            },
                            IterKind::SimdIter => {
                                end.push(
                                    BinOp {
                                        output: counter_sym.clone(),
                                        left: counter_sym.clone(),
                                        op: BinOpKind::Add,
                                        right:
                                        gen_sym_string(&SDACCEL_LOCAL_BUFF_SIZE_DEF.to_string()),
                                    }
                                );


                            },
                            _ => unimplemented!("Fringe Iter should not reach increment"),
                        };
                    }
                }

                let new_inner_block = prog.funcs[body_func].add_inner_block();

                match iter_kind {
                    IterKind::ScalarIter => {
                        //let lambda_counter = deref_sym(&gen_sym_size_sym(&builder_sym).clone());
                        let body_sym = gen_expr(body, prog, body_func, new_inner_block)?;
                        //Initialize data references
                        match lambda_var.ty {
                            Type::Scalar(_) => {
                                inner.push(
                                    // Setting builder reference
                                    InstantiateAssignIndexRight {
                                        left: lambda_var.name.clone(),
                                        ty: lambda_var.ty.clone(),
                                        right: var_syms[0].clone(),
                                        index: counter_sym.clone(),
                                    }
                                );
                            }
                            Type::Struct(vs) => {
                                for (i, v) in vs.iter().enumerate() {
                                    let var_i = struct_sym(&lambda_var.name, i);
                                    inner.push(
                                        InstantiateAssignIndexRight {
                                            left: var_i.clone(),
                                            ty: v.clone(),
                                            right: var_syms[i].clone(),
                                            index: counter_sym.clone(),
                                        }
                                    );
                                }
                            }
                            _ => return weld_err!("lambda var is neither scalar or struct")
                        }

                        //Copying things back. a little hacky
                        for s in &prog.funcs[body_func].blocks[new_inner_block].statements {
                            inner.push(s.clone());
                        }


                        let mut assign_back = Vec::new();
                        if let Type::Builder(ref bk, _) = builder.as_ref().ty {
                            match *bk {
                                BuilderKind::Merger(_, _) => {
                                    prog.assignments(&lambda_builder.name,
                                    &body_sym, &body.ty, &mut assign_back);
                                }
                                _ => {}
                            }
                        };
                        for s in &assign_back {
                            inner.push(s.clone());
                        }
                        prog.funcs[body_func].blocks[body_block].add_statement(
                            For {
                                init: init,
                                cond: cond,
                                end: end,
                                inner: inner,
                                attr: vec![Attribute::PipelineLoop],
                            }
                        );

                    },
                    IterKind::SimdIter => {

                        //allocate local buffs
                        let mut buffs_syms:Vec<Symbol> = Vec::new();
                        let buff_size_sym = gen_sym_string(
                                        &SDACCEL_LOCAL_BUFF_SIZE_DEF.to_string());

                        for (i, iter) in iters.iter().enumerate() {
                            let buff_sym = prog.add_local_buff(&iter.data.ty,
                                                    body_func)?;
                            buffs_syms.push(buff_sym.clone());

                            inner.push (
                                AsyncWorkGroupCopy {
                                    from: buff_sym.clone(),
                                    from_offset: gen_sym_string(&"0".to_string()),
                                    to: var_syms[i].clone(),
                                    to_offset: counter_sym.clone(),
                                    width: buff_size_sym.clone(),
                                    ty: iter.data.ty.clone(),
                                }
                            );
                        }

                        //Builder buff
                        //
                        let builder_buff_sym = prog.add_local_buff(&builder.ty,
                                                body_func)?;
                        buffs_syms.push(builder_buff_sym.clone());


                        // INNER FOR LOOP
                        //
                        let mut second_init:Vec<Statement> = Vec::new();
                        let mut second_cond:Vec<Statement> = Vec::new();
                        let mut second_end:Vec<Statement> = Vec::new();
                        let mut second_inner:Vec<Statement> = Vec::new();

                        let second_body_block = prog.funcs[body_func].add_inner_block();
                        let second_counter_sym = prog.add_local(&SDACCEL_COUNTER_TYPE,
                                                                body_func);


                        if let Type::Builder(ref bk, _) = builder.as_ref().ty {
                            match *bk {
                                BuilderKind::Appender(_) => {
                                    inner.push(
                                        Instantiate {
                                            left: gen_sym_inner_size_sym(&lambda_builder.name),
                                            ty: SDACCEL_COUNTER_TYPE.clone(),
                                        }
                                    );
                                    inner.push(
                                            AssignLiteral {
                                                output: gen_sym_inner_size_sym(&lambda_builder.name),
                                                value: LiteralKind::U32Literal(0),
                                            }
                                    );
                                    second_inner.push(
                                        LocalInstantiateAssign {
                                            left: lambda_builder.name.clone(),
                                            ty: builder.as_ref().ty.clone(),
                                            right: builder_buff_sym.clone(),
                                        }
                                    );
                                    //second_inner.push(
                                        //Assign {
                                            //output: gen_sym_inner_size_sym(&lambda_builder.name),
                                            //value: second_counter_sym.clone(),
                                        //}
                                    //);
                                }

                                BuilderKind::Merger(ref ty, _) => {
                                    inner.push(
                                        Instantiate {
                                            left: gen_sym_inner_sym(&lambda_builder.name.clone()),
                                            ty: ty.as_ref().clone(),
                                        }
                                    );
                                    inner.push(
                                        AssignLiteral {
                                            output: gen_sym_inner_sym(&lambda_builder.name.clone()),
                                            value: LiteralKind::U32Literal(0),
                                        }
                                    );
                                    //second_inner.push(
                                        //InstantiateAssign {
                                            //left: lambda_builder.name.clone(),
                                            //ty: builder.as_ref().ty.clone(),
                                            //right: builder_buff_sym.clone(),
                                        //}
                                    //);
                                }
                                _ => return weld_err!("Builder type not supported \
                                                      inside For")
                            }
                        }

                        //inner.push(
                            //InstantiateAssign {
                                //left: gen_sym_inner_size_sym(&deref_sym(&lambda_builder.name)),
                                //ty: SDACCEL_COUNTER_TYPE.clone(),
                                //right: ref_sym(&gen_sym_inner_size_sym(&lambda_builder.name)),
                            //}
                        //);


                        second_init.push(
                            AssignLiteral {
                                output: second_counter_sym.clone(),
                                value: LiteralKind::U32Literal(0),
                            }
                        );

                        second_cond.push(
                            BinOpNoAssign {
                                left: second_counter_sym.clone(),
                                right: buff_size_sym.clone(),
                                op: BinOpKind::LessThan,
                            }
                        );

                        second_end.push(
                            AssignStatement {
                                output: second_counter_sym.clone(),
                                right: Box::new(BinOpLiteralRight {
                                    left: second_counter_sym.clone(),
                                    op: BinOpKind::Add,
                                    right: LiteralKind::U32Literal(1),
                                })
                            }
                        );

                        match lambda_var.ty {
                            Type::Scalar(_) | Type::Simd(_) => {
                                second_inner.push(
                                    // Setting builder reference
                                    InstantiateAssignIndexRight {
                                        left: lambda_var.name.clone(),
                                        ty: lambda_var.ty.clone(),
                                        right: buffs_syms[0].clone(),
                                        index: second_counter_sym.clone(),
                                    }
                                );
                            }
                            Type::Struct(vs) => {
                                for (i, v) in vs.iter().enumerate() {
                                    let var_i = struct_sym(&lambda_var.name, i);
                                    second_inner.push(
                                        InstantiateAssignIndexRight {
                                            left: var_i.clone(),
                                            ty: v.clone(),
                                            right: buffs_syms[i].clone(),
                                            index: second_counter_sym.clone(),
                                        }
                                    );
                                }
                            }
                            _ => return weld_err!("lambda var is neither scalar or Simd")
                        }

                        //println!("body:{:?}", body);
                        let body_sym = gen_expr(body, prog, body_func, second_body_block)?;


                        //let mut assign_back = Vec::new();
                            //match *bk {
                                //BuilderKind::Merger(_, _) => {
                                    //prog.assignments(&lambda_builder.name,
                                    //&body_sym, &body.ty, &mut assign_back);
                                //}
                                //_ => {}
                            //}
                        //};

                        //Copying things back. a little hacky
                        for s in &prog.funcs[body_func].blocks[second_body_block].statements {
                            second_inner.push(s.clone());
                        }

                        //for s in &assign_back {
                            //second_inner.push(s.clone());
                        //}

                        inner.push(
                            For {
                                init: second_init,
                                cond: second_cond,
                                end: second_end,
                                inner: second_inner,
                                attr: vec![Attribute::UnrollHint]
                            }
                        );

                        if let Type::Builder(ref bk, _) = builder.as_ref().ty {
                            match *bk {
                                BuilderKind::Appender(_) => {
                                    inner.push(
                                        BinOp {
                                            output: deref_sym(&gen_sym_size_sym(&lambda_builder.name)),
                                            left: deref_sym(&gen_sym_size_sym(&lambda_builder.name)),
                                            op: BinOpKind::Add,
                                            right: gen_sym_inner_size_sym(&lambda_builder.name),
                                        }
                                    );
                                    // Copy result out
                                    inner.push(
                                        AsyncWorkGroupCopy {
                                            from: builder_sym.clone(),
                                            from_offset: counter_sym.clone(),
                                            to: builder_buff_sym.clone(),
                                            to_offset: gen_sym_string(&"0".to_string()),
                                            width: buff_size_sym.clone(),
                                            ty: builder.ty.clone(),
                                        }
                                    );
                                }

                                BuilderKind::Merger(ref ty, ref op) => {
                                    inner.push(BinOp {
                                                                 output: deref_sym(&lambda_builder.name),
                                                                 op: *op,
                                                                 left: deref_sym(&lambda_builder.name),
                                                                 right: gen_sym_inner_sym(&lambda_builder.name.clone()),
                                                             });
                                }
                                _ => return weld_err!("Builder type not supported \
                                                      inside For")
                            }
                        }
                        //inner.push(
                             //BinOp {
                                 //output: gen_sym_size_sym(&deref_sym(&lambda_builder.name)),
                                 //left: gen_sym_size_sym(&deref_sym(&lambda_builder.name)),
                                 //op: BinOpKind::Add,
                                 //right: gen_sym_string(&SDACCEL_LOCAL_BUFF_SIZE_DEF.to_string()),
                             //}
                        //);
                        prog.funcs[body_func].blocks[body_block].add_statement(
                            For {
                                init: init,
                                cond: cond,
                                end: end,
                                inner: inner,
                                //attr: vec![Attribute::PipelineLoop]
                                attr: vec![]
                            }
                        );

                    },
                    IterKind::FringeIter => {
                        unimplemented!()
                    }
                }



                // Generate function call
                let name = prog.funcs[body_func].func_name();
                let args = prog.funcs[body_func].params.clone();
                let buffs = prog.funcs[body_func].buffs.clone();

                // Creating arguments for the
                prog.funcs[cur_func].blocks[cur_block].add_statement(
                    Func {
                        name: name,
                        args: args.clone(),
                        buffs: buffs.clone(),
                        args_size: HashMap::new(),
                        buffs_size: HashMap::new(),
                });


                Ok(builder_sym)
            } else {
                return weld_err!("For body not lambda {}", print_expr(func));
            }


        }
        _ => {
            return weld_err!("Unsupported expression: {}\n{:?}", print_expr(expr), expr);
        }
    }

}