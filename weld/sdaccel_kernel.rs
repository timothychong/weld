
use std::fmt;

use std::collections::{BTreeMap, HashMap};
use super::ast::*;
use super::ast::LiteralKind::*;
use super::error::*;
use super::util::*;
use super::sdaccel_util::*;
use super::pretty_print::*;



pub enum Attribute {
    AlwaysInline,
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Attribute::AlwaysInline => write!(f, "always_inline")
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
            let mut res = format!("{}", v);
            // Hack to disambiguate from integers.
            if !res.contains(".") {
                res.push_str(".0");
            }
            res.push_str("F");
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

#[derive(Clone)]
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
        inner: Vec<Statement>
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

    InstantiateAssign{
        left: Symbol,
        ty: Type,
        right: Symbol
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
    }

    //IndexAssignRight {
        //output: Symbol,
        //value: Symbol,
        //index: Symbol,
    //}
}


impl Symbol {
    pub fn index (&self, i:String) -> Symbol{
        let mut s = self.clone();
        let name = s.name.clone();
        s.name.push_str(&format!("[{}]", i));
        s
    }
}

pub struct SDAccelKernel {

    pub funcs: Vec<SDAccelFunction>,
    pub sym_gen: SymbolGenerator,
    pub buffs: HashMap<Symbol, Type>,
    pub variable_map: HashMap<Symbol, Symbol>

    // This is for temporary storing things for code gen
}



impl SDAccelKernel {
    pub fn new() -> SDAccelKernel {
        let mut prog = SDAccelKernel {
            funcs: vec![],
            sym_gen: SymbolGenerator::new(),
            buffs: HashMap::new(),
            variable_map: HashMap::new()
        };
        //add main
        prog.add_func();
        prog
    }

    pub fn add_func(&mut self) -> FunctionId {
        let func = SDAccelFunction {
            id: self.funcs.len(),
            params: HashMap::new(),
            params_size: HashMap::new(),
            blocks: vec![],
            locals: HashMap::new(),
            attr: vec![],
            buffs: HashMap::new(),
            buffs_size: HashMap::new(),
        };
        self.funcs.push(func);
        self.funcs.len() - 1
    }

    pub fn sym_from_buffid(&self, id: BuffId) -> Symbol {
        Symbol {
            name: format!("{}{}", SDACCEL_BUFFER_BUILD_PREFIX, id),
            id: 0
        }
    }

    pub fn add_buff(&mut self, ty: &Type, func_id: FunctionId)
        -> WeldResult<(BuffId, Symbol)> {

        if let Type::Scalar(_) = *ty {
            let tyy = Type::Vector(Box::new(ty.clone()));
            let id = self.buffs.len();
            let sym = self.sym_from_buffid(id);
            self.funcs[func_id].buffs.insert(sym.clone(), tyy.clone());
            self.funcs[0].buffs.insert(sym.clone(), tyy.clone());
            self.buffs.insert(sym.clone(), tyy.clone());
            Ok((id, sym))
        } else {
            weld_err!("Error: add_buff not a scalar type")
        }
    }

    /// Add a local variable of the given type and return a symbol for it.
    pub fn add_local(&mut self, ty: &Type, func: FunctionId) -> Symbol {
        let sym = self.sym_gen.new_symbol(format!("fn{}_tmp", func).as_str());
        self.funcs[func].locals.insert(sym.clone(), ty.clone());
        sym
    }

    /// Add a local variable of the given type and name
    pub fn add_local_named(&mut self, ty: &Type, sym: &Symbol, func: FunctionId) {
        self.funcs[func].locals.insert(sym.clone(), ty.clone());
    }

    pub fn update_all_func_size_param(&mut self) {
        for func in &mut self.funcs {
            func.update_vec_with_size();
        }
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
    pub locals: HashMap<Symbol, Type>,
    pub params: HashMap<Symbol, Type>,
    pub attr: Vec<Attribute>,
    pub buffs: HashMap<Symbol, Type>,
    pub buffs_size: HashMap<Symbol, Type>,
    pub params_size: HashMap<Symbol, Type>,

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

    pub fn func_name(& self) -> String {
        format!("func_{}", self.id)
    }

    pub fn update_vec_with_size(&mut self) {
        let extra = extra_vec_with_size(&mut self.params);
        self.params_size.extend(extra);

        let extra = extra_vec_with_size(&mut self.buffs);
        self.buffs_size.extend(extra);

        for block in &mut self.blocks {
            for mut s in &mut block.statements{
                if let Statement::Func {
                    ref name,
                    ref args,
                    ref args_size,
                    ref buffs,
                    ref buffs_size
                } = s.clone() {
                    let args_size = extra_vec_with_size(args);
                    let buffs_size = extra_vec_with_size(buffs);
                    *s = Statement::Func {
                        args: args.clone(),
                        buffs: buffs.clone(),
                        name: name.clone(),
                        args_size: args_size,
                        buffs_size: buffs_size,
                    };
                }
            }
        }

    }

    pub fn find_sym_type(& self, sym: &Symbol) -> WeldResult<Type>{
        if let Some(x) = self.locals.get(&sym) {
            return Ok(x.clone())
        }
        if let Some(x) = self.params.get(&sym) {
            return Ok(x.clone())
        }
        if let Some(x) = self.buffs.get(&sym) {
            return Ok(x.clone())
        }
        weld_err!("Find_Sym_Type not found")
    }

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
            } => write!(f, "{} = {}", output, print_literal(value)),
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
                if sorted_buffs.len() > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", join("", ", ", "", sorted_buffs.iter().map(|e| format!("{}", e.0))))?;
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
            } => {
                write!(f, "__attribute__((xcl_pipeline_loop))\nfor({}; {}; {}){{\n{}\n  }}",
                    join("", ", ", "", init.iter().map(|e| format!("{}", e))),
                    join("", ", ", "", cond.iter().map(|e| format!("{}", e))),
                    join("", ", ", "", end.iter().map(|e| format!("{}", e))),
                    join("", ";\n", ";\n", inner.iter().map(|e| format!("\t{}", e))),
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
       //Params here
        let params_sorted: BTreeMap<&Symbol, &Type> = self.params.iter().collect();
       write!(f, "{}", join("(", ", ", "",
               params_sorted.iter().map(
                   |e| format!("__global {} {}", gen_scalar_type(e.1).unwrap(), e.0)
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
       write!(f, "{}", join("", ", ", "",
               buffs_sorted.iter().map(
                   |e| format!("__global {} {}", gen_scalar_type(e.1).unwrap(), e.0)
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

        write!(f, "){{\n\n")?;

       if self.id == 0 {
           let buffsize_sorted: BTreeMap<&Symbol, &Type> = self.buffs_size.iter().collect();
            for (name, ty) in buffsize_sorted {
                write!(f, "  {} {};\n", print_type(ty), name)?;
            }
       }

        let locals_sorted: BTreeMap<&Symbol, &Type> = self.locals.iter().collect();
        for (name, ty) in locals_sorted {
            write!(f, "  {} {};\n", print_type(ty), name)?;
        }
        for block in &self.blocks {
            if !block.is_inner_block {
                write!(f, "{}", block)?;
            }
        }
        write!(f, "\n}}")?;
        Ok(())
    }
}

impl fmt::Display for SDAccelKernel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
                ) -> WeldResult<Option<Symbol>> {

    use self::Statement::*;
    match expr.kind {

        ExprKind::Ident(ref sym) => Ok(Some(sym.clone())),

        ExprKind::Literal(lit) => {
            let res_sym = prog.add_local(&expr.ty, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(AssignLiteral {
                                                                     output: res_sym.clone(),
                                                                     value: lit,
                                                                 });
            //prog.assign_var_map(&res_sym, lit);
            Ok(Some(res_sym))
        }

        ExprKind::Let {
            ref name,
            ref value,
            ref body,
        } => {
            let val = gen_expr(value, prog, cur_func, cur_block).unwrap().unwrap();
            prog.add_local_named(&value.ty, name, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(Assign {
                                                                     output: name.clone(),
                                                                     value: val.clone(),
                                                                 });

            //Create alias for vector size
            match value.ty {
                Type::Vector(_) => {
                    let name = gen_sym_size_sym(name);
                    let val = gen_sym_size_sym(&val);
                    prog.add_local_named(&SDACCEL_SIZE_TYPE, &name, cur_func);
                prog.funcs[cur_func].blocks[cur_block].add_statement(Assign {
                                                                         output: name.clone(),
                                                                         value: val.clone(),
                                                                     });

                }
                _ => {}
            }
            gen_expr(body, prog, cur_func, cur_block)
        }

        ExprKind::BinOp {
            kind,
            ref left,
            ref right,
        } => {
            let left_sym = gen_expr(left, prog, cur_func, cur_block)?.unwrap();
            let right_sym = gen_expr(right, prog, cur_func, cur_block)?.unwrap();
            let res_sym = prog.add_local(&expr.ty, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(BinOp {
                                                                     output: res_sym.clone(),
                                                                     op: kind,
                                                                     left: left_sym,
                                                                     right: right_sym,
                                                                 });
            Ok(Some(res_sym))
        }

        ExprKind::UnaryOp {
            kind,
            ref value,
        } => {
            let value_sym = gen_expr(value, prog, cur_func, cur_block)?.unwrap();
            let res_sym = prog.add_local(&expr.ty, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(UnaryOp {
                output: res_sym.clone(),
                op: kind,
                child: value_sym,
            });
            Ok(Some(res_sym))
        }

        //ExprKind::Negate(ref child_expr) => {
            //println!("Negate")
        //}

        //ExprKind::Broadcast(ref child_expr) => {
            //println!("Broadcast")
        //}

        //ExprKind::Cast { ref child_expr, .. } => {
            //println!("Cast")
        //}

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

        //ExprKind::If {
            //ref cond,
            //ref on_true,
            //ref on_false,
        //} => {
            //println!("If")
        //}

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
            let res_sym = gen_expr(value, prog, cur_func, cur_block)?;
            Ok(res_sym)
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
                        if let Type::Vector(ref ty) = data.ty{
                            let (_ , nbuilder) = prog.add_buff(&(*ty), cur_func)?;
                            //println!("buff: {:?} e: {:?}", nbuilder, e);

                            //For now always create a new buffer
                            Ok(Some(nbuilder))

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
                                let (_ , nbuilder) = prog.add_buff(&*t, cur_func)?;
                                return Ok(Some(nbuilder))
                            }
                            _ => return weld_err!("Builder type not supported: {}", print_expr(expr))
                        }
                    } else {
                        return weld_err!("New Builder Type not a builder")
                    }
                }

            }
        }

        //ExprKind::MakeStruct { ref elems } => {
            //println!("Make struct")
        //}

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
            let mut res_sym = gen_expr(expr, prog, cur_func, cur_block)?.unwrap();
            // Combining id ito field
            res_sym.name = format!("{}_{}", res_sym.name, index);
            Ok(Some(res_sym))

        }

        ExprKind::For {
            ref iters,
            ref builder,
            ref func,
        } => {

            match iters[0].kind {
                IterKind::ScalarIter => {
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

                        let builder_sym = gen_expr(builder, prog, body_func, body_block)?.unwrap();
                        let build_ty = prog.funcs[body_func].buffs.get(&builder_sym).unwrap().clone();

                        let lambda_builder = params[0].clone();
                        let lambda_counter = deref_sym(&gen_sym_size_sym(&builder_sym).clone());
                        let lambda_var = params[2].clone();

                        let mut var_syms:Vec<Symbol> = Vec::new();
                        let mut counter_syms:Vec<Symbol> = Vec::new();

                        // For each item in the iterators
                        for iter in iters {
                            match iter.kind {
                                IterKind::ScalarIter => {
                                    let before_data = gen_expr(&iter.data, prog, cur_func, cur_block)?;


                                    //Get the symbol for the data
                                    //let data:Symbol;
                                    //match before_data {
                                        //None => {
                                            ////TODO
                                            //println!("ITER: {}", print_expr(&iter.data));
                                            //data = Symbol {
                                                //name:format!("FOO"),
                                                //id:0,
                                            //};
                                        //}
                                        //Some(d) => {

                                            //data = d;
                                        //}
                                    //}
                                    let data:Symbol = before_data.unwrap();

                                    // Get the symbol and then add it to the parameter
                                    //let ty = match prog.funcs[body_func].locals.get(&data){
                                        //// If you find it in local var.
                                        //// Do you still need to add it to params?
                                        //Some(d) => d.clone(),
                                        //// Look for it in global
                                        //None => {
                                            //prog.funcs[0].find_sym_type(&data)?
                                        //}
                                    //};
                                    let ty = prog.funcs[0].find_sym_type(&data)?;
                                    var_syms.push(data.clone());
                                    prog.funcs[body_func].params.insert(data.clone(), ty.clone());

                                    prog.assign_var_map(
                                        &data,
                                        &builder_sym,
                                    );

                                    //Create counter for iterator
                                    let counter_sym = prog.add_local(&SDACCEL_COUNTER_TYPE, body_func);
                                    //let counter_sym = &counter_sym_before_deref;
                                    //let counter_sym = deref_sym(&gen_sym_size_sym(&builder_sym));

                                    //Right now only assume loop through entire array
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
                                    counter_syms.push(counter_sym);


                                }
                                _ => return weld_err!("Only ScalarIter is supported: {}", print_expr(func))
                            }
                        }



                        //prog.funcs[body_func].add_func_counter(&lambda_counter.name);
                        //
                        let new_inner_block = prog.funcs[body_func].add_inner_block();
                        let res_sym = gen_expr(body, prog, body_func, new_inner_block)?.unwrap();

                        //println!("YO: {:?}", body);

                        let res_ty = prog.funcs[body_func].locals.get(&res_sym).unwrap().clone();

                        //Initialize builder reference
                        prog.funcs[body_func].blocks[body_block].add_statement(
                            // Setting builder reference
                            GlobalInstantiateAssign {
                                left: lambda_builder.name.clone(),
                                ty: build_ty.clone(),
                                right: builder_sym.clone(),
                            }
                        );

                        //Initialize data references
                        match lambda_var.ty {
                            Type::Scalar(ty) => {
                                inner.push(
                                    // Setting builder reference
                                    InstantiateAssignIndexRight {
                                        left: lambda_var.name.clone(),
                                        ty: lambda_var.ty.clone(),
                                        right: var_syms[0].clone(),
                                        index: counter_syms[0].clone(),
                                    }
                                );
                            }
                            Type::Struct(vs) => {
                                for (i, v) in vs.iter().enumerate() {
                                    let mut name_pre = lambda_var.name.clone();
                                    name_pre.name.push_str(&format!("_{}", i));
                                    inner.push(
                                        InstantiateAssignIndexRight {
                                            left: name_pre.clone(),
                                            ty: v.clone(),
                                            right: var_syms[i].clone(),
                                            index: counter_syms[i].clone(),
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

                        // Assigning output
                        inner.push(
                            IndexAssignLeft{
                                output: lambda_builder.name.clone(),
                                index: lambda_counter.clone(),
                                value: res_sym
                            }
                        );
                        //Builder iterator
                        init.push(
                            AssignLiteral {
                                output: lambda_counter.clone(),
                                value: LiteralKind::U32Literal(0),
                            }
                        );

                        end.push(
                            AssignStatement {
                                output: lambda_counter.clone(),
                                right: Box::new(BinOpLiteralRight {
                                    left: lambda_counter.clone(),
                                    op: BinOpKind::Add,
                                    right: LiteralKind::U32Literal(1),
                                })
                            }
                        );

                        prog.funcs[body_func].blocks[body_block].add_statement(
                            For {
                                init: init,
                                cond: cond,
                                end: end,
                                inner: inner,
                            }
                        );

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


                        Ok(Some(builder_sym))

                    } else {
                        weld_err!("Argument to For was not a Lambda inside For: {}", print_expr(func))
                    }

                },
                _ => return weld_err!("Only ScalarIter is supported: {}", print_expr(func))
            }
        }
        _ => {
            return weld_err!("Unsupported expression: {}", print_expr(expr));
        }
    }

}
