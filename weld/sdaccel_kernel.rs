
use std::fmt;

use std::collections::{BTreeMap, HashMap};
use super::ast::*;
use super::ast::LiteralKind::*;
use super::error::*;
use super::util::SymbolGenerator;
use super::sdaccel_util::*;


pub type BasicBlockId = usize;
pub type FunctionId = usize;
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

pub fn print_literal(lit: &LiteralKind) -> String {
    match *lit {
        BoolLiteral(v) => format!("{}", v),
        I8Literal(v) => format!("{}", v),
        I16Literal(v) => format!("{}", v),
        I32Literal(v) => format!("{}", v),
        I64Literal(v) => format!("{}L", v),
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
}

pub struct SDAccelKernel {

    pub funcs: Vec<SDAccelFunction>,

    sym_gen: SymbolGenerator,
}


impl SDAccelKernel {
    pub fn new() -> SDAccelKernel {
        let mut prog = SDAccelKernel {
            funcs: vec![],
            sym_gen: SymbolGenerator::new(),
        };
        /// add main
        prog.add_func();
        prog
    }

    pub fn add_func(&mut self) -> FunctionId {
        let func = SDAccelFunction {
            id: self.funcs.len(),
            params: HashMap::new(),
            blocks: vec![],
            locals: HashMap::new(),
        };
        self.funcs.push(func);
        self.funcs.len() - 1
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

}

pub struct SDAccelFunction {
    pub id: FunctionId,
    pub blocks: Vec<BasicBlock>,
    pub locals: HashMap<Symbol, Type>,
    pub params: HashMap<Symbol, Type>,
}

impl SDAccelFunction {

    pub fn add_block(&mut self) -> BasicBlockId {
        let block = BasicBlock {
            id: self.blocks.len(),
            statements: vec![],
        };
        self.blocks.push(block);
        self.blocks.len() - 1
    }
}

#[derive(Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub statements: Vec<Statement>,
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
                       op,
                       left,
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
        write!(f, "{{\n")?;
        //write!(f, "Params:\n")?;
        //let params_sorted: BTreeMap<&Symbol, &Type> = self.params.iter().collect();
        //for (name, ty) in params_sorted {
            //write!(f, "{} {};\n", gen_scalar_type(ty).unwrap(), name)?;
        //}
        let locals_sorted: BTreeMap<&Symbol, &Type> = self.locals.iter().collect();
        for (name, ty) in locals_sorted {
            write!(f, "  {} {};\n", gen_scalar_type(ty).unwrap(), name)?;
        }
        for block in &self.blocks {
            write!(f, "{}", block)?;
        }
        write!(f, "\n}}")?;
        Ok(())
    }
}

impl fmt::Display for SDAccelKernel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in &self.funcs {
            write!(f, "{}\n", func)?;
        }
        Ok(())
    }
}

/// Generate code to compute the expression `expr`
pub fn gen_expr(expr: &TypedExpr,
                prog: &mut SDAccelKernel,
                cur_func: FunctionId,
                cur_block: BasicBlockId,
                ) -> WeldResult<(FunctionId, BasicBlockId, Symbol)> {

    use self::Statement::*;
    match expr.kind {

        ExprKind::Ident(ref sym) => Ok((cur_func, cur_block, sym.clone())),

        ExprKind::Literal(lit) => {
            let res_sym = prog.add_local(&expr.ty, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(AssignLiteral {
                                                                     output: res_sym.clone(),
                                                                     value: lit,
                                                                 });
            Ok((cur_func, cur_block, res_sym))
        }

        ExprKind::Let {
            ref name,
            ref value,
            ref body,
        } => {
            let (cur_func, cur_block, val_sym) = gen_expr(value, prog, cur_func, cur_block)?;
            prog.add_local_named(&value.ty, name, cur_func);
            prog.funcs[cur_func].blocks[cur_block].add_statement(Assign {
                                                                     output: name.clone(),
                                                                     value: val_sym,
                                                                 });
            let (cur_func, cur_block, res_sym) = gen_expr(body, prog, cur_func, cur_block)?;
            Ok((cur_func, cur_block, res_sym))
        }

        //ExprKind::BinOp {
            //kind,
            //ref left,
            //ref right,
        //} => {
            //println!(" BiOp")
        //}

        //ExprKind::UnaryOp {
            //kind,
            //ref value,
        //} => {
            //println!("UnaryOp")
        //}

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
            //println!("Lookup")
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

        //ExprKind::Length { ref data } => {
            //println!("Length")
        //}

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

        //ExprKind::Merge {
            //ref builder,
            //ref value,
        //} => {
            //println!("Merge")
        //}

        //ExprKind::Res { ref builder } => {
            ////println!("Res builder kind: {}", print_expr(expr));
            //println!("Res builder kind");
            //if let Type::Builder(ref kind, _) = builder.ty {
                ////gen_expr(builder);
            //} else {
                ////weld_err!("Res of not a builder: {}", print_expr(builder))
            //}
        //}

        //ExprKind::NewBuilder(ref arg) => {
            //println!("New builder")
        //}

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

        //ExprKind::GetField { ref expr, index } => {
            //println!("GetField")
        //}

        ExprKind::For {
            ref iters,
            ref builder,
            ref func,
        } => {
            if let ExprKind::Lambda {
                       ref params,
                       ref body,
                   } = func.kind {
                let body_block = prog.funcs[body_func].add_block();



                Ok((cont_func, cont_block, builder_sym))
            } else {
                weld_err!("Argument to For was not a Lambda: {}", print_expr(func))
            }
        }
        _ => println!("Unsupported expression: {}", print_expr(expr)),
        _ => {
            println!("Unsupported expression: ");
            Ok((cur_func, cur_block, Symbol{name: "foo".to_string(), id:0}))
        }
    }

}
