use wasm::ast::{BlockType, InstrType, Idx, Memarg};
//use wasm::ast::highlevel::{GlobalOp::*, LocalOp::*};
//use wasm::ast::highlevel::{Function, GlobalOp::*, Instr, Instr::*, LocalOp::*, Module};
use wasm::ast::highlevel::Instr;

use wasm::ast::highlevel::NumericOp;


use self::TaintStackElement::*;

use std::collections::HashMap;

use super::block_stack;



#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct OpAndInputs (NumericOp, Vec<TaintType>);

type OpChain = Vec<OpAndInputs>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct InputSizeFormula (OpChain);


/*
fn clone_input_size_formula(orig: &InputSizeFormula) -> InputSizeFormula {
    let new_vec = orig.0.iter().map(|&val| {
            let new_var = match val {
                InputVar::LocalVar(l_idx) => {
                    InputVar::LocalVar(l_idx.clone())
                },
                InputVar::GlobalVar(g_idx) => {
                    InputVar::GlobalVar(g_idx.clone())
                },
                InputVar::Constant => InputVar::Constant,
                InputVar::MemoryVal => InputVar::MemoryVal,
                InputVar::InputSize => InputVar::InputSize,
                InputVar::Undetermined => InputVar::Undetermined,
            };
            new_var
     }).collect();

    new_vec
}
*/


// TaintType enum is ordered by dominance. when two values are combined in a wasm instruction, the output type is the dominant type.
// so InputVal can taint anything lower (inputSize and Constant)
// for a value to remain constant, it must only ever be tainted by other constants
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum TaintType {
    Constant(wasm::ast::Val), // value is derived only from constant values
    InputSize, // value is derived from getCallDataSize or constant values
    //InputSize(OpChain), // this has a Vec, which prevents using Copy trait, which creates a world of borrowing errors
    InputVal, // value is derived from input data values
    Undetermined, // value is from a function that hasn't been analyzed yet
    MemoryVal, // temporary until memory taint tracking is implemented. just lets us know what we're missing
}


#[derive(Debug)]
pub struct GlobalTaintMap (HashMap<Idx<wasm::ast::highlevel::Global>, TaintType>);

impl<'lt> GlobalTaintMap {
    pub fn new() -> Self {
        println!("GlobalTaintMap.new!");
        let global_taints = HashMap::new();
        GlobalTaintMap(global_taints)
    }

    pub fn print_global_taints(&mut self) {
        println!("global taints: {:?}", self.0)
    }
}



type InstrPosition = Idx<Instr>;

#[derive(Debug)]
pub struct CallToFunc {
    source_idx: usize,
    target_idx: usize,
    input_taints: Vec<TaintType>,
    call_pos: InstrPosition,
}


#[derive(Debug)]
pub struct CallGraph (Vec<CallToFunc>);


impl<'lt> CallGraph {
    pub fn new() -> CallGraph {
        CallGraph(Vec::new())
    }

    pub fn push(&mut self, call_to_func: CallToFunc) {
        self.0.push(call_to_func);
    }

    pub fn print(self) {
        for call in self.0 {
            println!("{:?}", call);
        }
        //println!("{:?}\n", self.0);
        println!("\n");
    }
}




#[derive(Debug)]
pub struct BrToLoop {
    br_conditional_var: TaintType,
    br_position: InstrPosition,
    br_target: block_stack::BranchTarget,
}

#[derive(Debug)]
pub struct LoopControllers (Vec<BrToLoop>);


impl LoopControllers {
    fn new() -> LoopControllers {
        LoopControllers(Vec::new())
    }

    fn push(&mut self,br_to_loop: BrToLoop) {
        self.0.push(br_to_loop);
    }
}


// using usize instead of idx::Function baloney
#[derive(Debug)]
pub struct ModFuncLoopControls(HashMap<usize, LoopControllers>);

impl<'lt> ModFuncLoopControls {
    pub fn new() -> Self {
        println!("ModFuncLoopControls.new!");
        let mod_func_loop_controls = HashMap::new();
        ModFuncLoopControls(mod_func_loop_controls)
    }
    
    pub fn get_or_init_func_entry(&mut self, fn_id: usize) -> &mut LoopControllers {
        if !self.0.contains_key(&fn_id) {
            self.0.insert(fn_id, LoopControllers::new());
        }

        self.0.get_mut(&fn_id).unwrap()
    }

    pub fn add_loop_br_control(&mut self, fn_id: usize, br_to_loop: BrToLoop) {
        // get current taint var...
        println!("ModFuncLoopControls add_loop_br_control for fn_id {:?}.  br_to_loop: {:?}", fn_id, br_to_loop);
        self.get_or_init_func_entry(fn_id).push(br_to_loop);
    }

    pub fn print_all_loop_controls(&mut self) {
        //println!("all loop controls: {:?}", self.0)
        println!("");
        println!("--------------------------------------------");
        println!("function \"loop controls\". for each br_if that branches to a loop (branch target is a loop instruction), the conditional var determines how many iterations of the loop will be executed.");
        println!("If all the loop-targeting br_ifs have conditional vars that are taint class `constant` (i.e. are derived from propagated constants), then the runtime is constant for all inputs. loops can be unrolled during metering and a simple gas formula is easy to generate (ideal case for upper-bound analysis)");
        println!("If a loop-targeting br_if has a conditional var that is taint class `inputSize`, then runtime is dependendent on input length, i.e. only input length and not on input value (second-best case for upper-bound analysis)");
        println!("If a loop-targeting br_if has a conditional var that is taint calss `inputVal`, then runtime is dependent on input value (worst-case complexity, most difficult to find upper bound)");
        println!("note that functions other than main() are still inaccurate, because we aren't yet propagating taints across functions. Right now `InputVal` means it is tainted by a function input param. But all functions are ultimately called from main(), so their input params' taints should flow from the vars passed from main. We need to propagate taints from main() to all other functions and back, in order to see the correct taint of loop-targeting br_ifs in all other functions. TODO: fix this.\n");

        let mut has_constant_infinite_loop = false;
        let mut key_nums = Vec::new();
        for key in self.0.keys() {
            key_nums.push(key);
        }
        key_nums.sort();
        for key in key_nums {
            //let fidx_typed = Idx::<wasm::ast::highlevel::Function>::from(key);
            let loop_controls = self.0.get(&key).unwrap();
            for control in &loop_controls.0 {
                println!("function {:?} loop control: {:?}", key, control);

                match control.br_conditional_var {
                    TaintType::Constant(v) => {
                        match v {
                            wasm::ast::Val::I32(1) => {
                                println!("DANGER will robinson! wasm has an infinite loop regardless of input!! do not run!!!");
                            },
                            _ => {}
                        }
                    },
                    _ => {},
                } // done with constant infinite loop check

            }
            
        }
        
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum TaintTypeOrFormula {
    TaintedVar(TaintType),
    InputSizeFormula(OpChain),
}




impl From<&mut TaintTypeOrFormula> for TaintType {
    fn from(ttof: &mut TaintTypeOrFormula) -> TaintType {
        match ttof {
            TaintTypeOrFormula::TaintedVar(taint_ty) => {
                *taint_ty
            },
            TaintTypeOrFormula::InputSizeFormula(_opchain) => {
                println!("converting InputSizeFormula to InputSize.  NONO");
                TaintType::InputSize
            }
        }
    }
}


impl From<TaintTypeOrFormula> for TaintType {
    fn from(ttof: TaintTypeOrFormula) -> TaintType {
        match ttof {
            TaintTypeOrFormula::TaintedVar(taint_ty) => {
                taint_ty
            },
            TaintTypeOrFormula::InputSizeFormula(_opchain) => {
                println!("converting InputSizeFormula to inputSize. NONO");
                TaintType::InputSize
            }
        }
    }
}

impl From<TaintType> for TaintTypeOrFormula {
    fn from(tt: TaintType) -> TaintTypeOrFormula {
        match tt {
            taint_ty => TaintTypeOrFormula::TaintedVar(taint_ty)
        }
    }
}



/*
fn clone_tainttype_or_formula(orig: &TaintTypeOrFormula) -> TaintTypeOrFormula {
    let new_vec = orig.0.iter().map(|&val| {
            let new_var = match val {
                InputVar::LocalVar(l_idx) => {
                    InputVar::LocalVar(l_idx.clone())
                },
                InputVar::GlobalVar(g_idx) => {
                    InputVar::GlobalVar(g_idx.clone())
                },
                InputVar::Constant => InputVar::Constant,
                InputVar::MemoryVal => InputVar::MemoryVal,
                InputVar::InputSize => InputVar::InputSize,
                InputVar::Undetermined => InputVar::Undetermined,
            };
            new_var
     }).collect();

    new_vec
}
*/






#[derive(Debug)]
pub struct TaintStack<'lt> (Vec<TaintStackElement>, HashMap<Idx<wasm::ast::Local>, TaintTypeOrFormula>, &'lt mut GlobalTaintMap, &'lt mut ModFuncLoopControls, &'lt mut CallGraph);

#[derive(Debug, PartialEq)]
enum TaintStackElement {
    TaintedVar(TaintType),
    BlockBegin(BlockType),
    FunctionBegin,
// TODO see add_hooks/mod.rs
//    Unreachable,
}

impl TaintStackElement {
    pub fn to_taint(&self) -> TaintType {
        match self {
            TaintedVar(taint_ty) => *taint_ty,
            BlockBegin(_block_ty) => TaintType::Undetermined,
            FunctionBegin => TaintType::Undetermined,
            /*
            TaintStackElement::InputSizeFormula(_op_chain) => {
                println!("TaintStackElement InputSizeFormula converting to InputSize. NONO");
                TaintType::InputSize
            },
            */
        }
    }

    pub fn to_taint_or_formula(&self) -> TaintTypeOrFormula {
        match self {
            TaintedVar(taint_ty) => TaintTypeOrFormula::TaintedVar(*taint_ty),
            // TODO: panic on blockbegin and functionbegin?
            BlockBegin(_block_ty) => TaintType::Undetermined.into(),
            FunctionBegin => TaintType::Undetermined.into(),
            /*
            TaintStackElement::InputSizeFormula(_op_chain) => {
                //println!("TaintStackElement InputSizeFormula converting to InputSize. NONO");
                TaintTypeOrFormula::InputSizeFormula(_op_chain.to_vec())
            },
            */
        }
    }
}


fn do_binary_op(op: wasm::ast::highlevel::NumericOp, a: wasm::ast::Val, b: wasm::ast::Val) -> wasm::ast::Val {
    match op {
        wasm::ast::highlevel::NumericOp::I32Add => {
            match (a, b) {
                (wasm::ast::Val::I32(a), wasm::ast::Val::I32(b)) => {
                    wasm::ast::Val::I32(a + b)
                },
                _ => panic!("should never happen on validated module!")
            }
        },
        wasm::ast::highlevel::NumericOp::I32Sub => {
            match (a, b) {
                (wasm::ast::Val::I32(a), wasm::ast::Val::I32(b)) => {
                    // TODO: check that a - b is right and not b - a
                    wasm::ast::Val::I32(a - b)
                },
                _ => panic!("should never happen on validated module!")
            }
        },
        wasm::ast::highlevel::NumericOp::I32Shl => {
            match (a, b) {
                (wasm::ast::Val::I32(a), wasm::ast::Val::I32(b)) => {
                    // TODO: check that order is right
                    use std::ops::Shl;
                    let mask = 0x1F;
                    wasm::ast::Val::I32(a.shl(b & mask))
                },
                _ => panic!("should never happen on validated module!")
            }
        },
        wasm::ast::highlevel::NumericOp::I32LtU => {
            match (a, b) {
                (wasm::ast::Val::I32(a), wasm::ast::Val::I32(b)) => {
                    // TODO: check that order is right
                    if (a < b) {
                        wasm::ast::Val::I32(1)
                    } else {
                        wasm::ast::Val::I32(0)
                    }
                    
                },
                _ => panic!("should never happen on validated module!")
            }
        },
        _ => panic!("unimplemented binary op! {:?}", op)
    }
}

//fn propagateConstVals(stack_els: )


fn taint_variable(a: TaintType, b: TaintType) -> TaintType {
    //let result = (a, b);
    match (a, b) {
        _ if a > b => a, // a dominates b
        _ if a < b => b, // b dominates a
        _ => a, // both equal
    }
    /*
    match (a, b) {
        (TaintType::Constant, TaintType::Constant) => TaintType::Constant,
        (TaintType::Constant, TaintType::InputSize) => TaintType::InputSize,
        (TaintType::Constant, TaintType::InputVal) => TaintType::InputVal,
        (TaintType::InputSize, TaintType::Constant) => TaintType::InputSize,
        (TaintType::InputSize, TaintType::InputSize) => TaintType::InputSize,
        (TaintType::InputSize, TaintType::InputVal) => TaintType::InputVal,
        (TaintType::InputVal, TaintType::Constant) => TaintType::InputVal,
        (TaintType::InputVal, TaintType::InputSize) => TaintType::InputVal,
        (TaintType::InputVal, TaintType::InputVal) => TaintType::InputVal,
        (TaintType::MemoryVal, TaintType::InputVal) => TaintType::MemoryVal,
        _ => TaintType::Undetermined,
    }
    */
}



impl<'lt> TaintStack<'lt> {
    pub fn new(global_taint_map: &'lt mut GlobalTaintMap, mod_func_loop_controls: &'lt mut ModFuncLoopControls, call_graph: &'lt mut CallGraph) -> Self {
        println!("TaintStack.new!");
        let local_taints = HashMap::new();
        //let global_taints = global_taint_map
        TaintStack(vec![FunctionBegin], local_taints, global_taint_map, mod_func_loop_controls, call_graph)
    }

    pub fn push_val(&mut self, ttof: TaintTypeOrFormula) {
        println!("TaintStack.push_val ttof {:?}", ttof);
        self.0.push(TaintedVar(ttof.into()))
        //self.0.push(ttof)
        //self.0.push(ty)
    }

    pub fn print_taint_stack(&mut self) {
        println!("taint stack: {:?}", self.0);
        //println!("top of stack: {:?}", self.0.last().unwrap());
    }

    pub fn add_loop_br_control(&mut self, fn_id: usize, br_position: InstrPosition, br_target: block_stack::BranchTarget) {
        // get current taint var...
        println!("TaintStack add_loop_br_control for fn_id {:?}.  br_position: {:?}   br_target: {:?}", fn_id, br_position, br_target);
        //br_to_loop: BrToLoop
        let br_conditional_var = self.0.last().unwrap();
        let br_conditional_taint_type = br_conditional_var.to_taint();
        println!("br to loop conditional var has taint: {:?}", br_conditional_taint_type);
        let br_to_loop = BrToLoop {
            br_position: br_position,
            br_target: br_target,
            br_conditional_var: br_conditional_taint_type,
        };
        self.3.add_loop_br_control(fn_id, br_to_loop);
    }

    /// panics if stack is empty or if there was a block begin (and not a ValType)
    pub fn pop_val(&mut self) -> TaintTypeOrFormula {
        println!("TaintStack.pop_val...");
        match self.0.pop() {
            None => panic!("tried to pop from empty type stack"),
            Some(TaintedVar(taint_ty)) => taint_ty.into(),
            //Some(TaintStackElement::InputSizeFormula(opchain)) => TaintTypeOrFormula::InputSizeFormula(opchain),
            Some(BlockBegin(_)) => panic!("expected ValType on type stack, but got block begin marker indicating empty block stack; full type stack was {:?}", self.0),
            Some(FunctionBegin) => panic!("expected ValType on type stack, but got function begin marker indicating empty block stack; full type stack was {:?}", self.0),
        }
    }

    pub fn memory_store_instr(&mut self, op: wasm::ast::highlevel::StoreOp, memarg: wasm::ast::Memarg ) {
        // mem store ops pop two vals off the stack and push 0

        // TODO: track memory offset taints

        // the num of bytes of in memory that will be written to is dependent on the store op type (store8, store16, etc.)
        // and the type of the val to store (i32 is 4 bytes, i64 is 8 bytes)
        // if op is store8, then N=8 and the value = v mod 2**N (store first byte of the i32 or i64)
        // if op is generic store (N is not part of the instruction), then num of bytes is just the argument size (i32 or i64)

        let _val_to_store = self.pop_val();
        let _i_offset = self.pop_val();
        // actual offset is i + memarg.offset

        println!("TaintStack.memory_store memarg: {:?}", memarg);
        println!("TaintStack.memory_store _i_offset: {:?}", _i_offset);
    }

    pub fn memory_load_instr(&mut self, op: wasm::ast::highlevel::LoadOp, memarg: wasm::ast::Memarg ) {
        // mem loads pop one val off the stack and push one
        let _i_offset = self.pop_val();
        // memory position is i + memarg.offset
        // TODO: get taint of memory position
        self.push_val(TaintType::MemoryVal.into());
    }

    pub fn local_instr(&mut self, op: wasm::ast::highlevel::LocalOp, local_idx: Idx<wasm::ast::Local>) {
        println!("TaintStack.local_instr...");
        match op {
            wasm::ast::highlevel::LocalOp::GetLocal => {
                //self.pop_val();
                if self.1.contains_key(&local_idx) {
                    let local_taint = self.1.get(&local_idx).unwrap();
                    println!("local var has taint: {:?}.  pushing to taint stack..", local_taint);
                    let cloned_taint = local_taint.clone();
                    self.push_val(cloned_taint);
                } else {
                    // if the local hasn't been previously set with SetLocal, then it is an input param.
                    println!("getting unset local, must be an input param. pushing to taint stack {:?}", TaintType::Undetermined);
                    self.push_val(TaintType::Undetermined.into());
                }
            },
            wasm::ast::highlevel::LocalOp::SetLocal => {
                let taint_elem = self.pop_val();
                println!("setting local. stack var has taint: {:?}", taint_elem);
                // TODO: TaintTypeOrFormula .into() TaintType here..
                // want to preserve formula
                self.1.insert(local_idx, taint_elem.into());
            },
            wasm::ast::highlevel::LocalOp::TeeLocal => {
                /*
                let taint_elem = self.0.last().unwrap();
                let tainted_var_ty = taint_elem.to_taint();
                println!("teeing local. stack var has taint: {:?}", tainted_var_ty);
                self.1.insert(local_idx, tainted_var_ty);
                */
                let taint_elem = self.0.last().unwrap();
                //let tainted_var_ty = taint_elem.to_taint();
                println!("teeing local. stack var has taint: {:?}", taint_elem);
                self.1.insert(local_idx, taint_elem.to_taint_or_formula());
                /*
                match *taint_elem {
                    TaintType => {
                        self.1.insert(local_idx, *taint_elem);
                    },
                    _ => {},
                }
                */
            }
        }
    }

    pub fn global_instr(&mut self, op: wasm::ast::highlevel::GlobalOp, global_idx: Idx<wasm::ast::highlevel::Global>) {
        println!("TaintStack.global_instr  {:?}  {:?}", op, global_idx);
        //let global_taints = 
        match op {
            wasm::ast::highlevel::GlobalOp::GetGlobal => {
                //self.pop_val();
                // FIXME: need to pop val??
                //if self.2.contains_key(&global_idx) {
                if (self.2).0.contains_key(&global_idx) {
                    let global_taint = (self.2).0.get(&global_idx).unwrap();
                    println!("global var has taint: {:?}.  pushing to taint stack..", global_taint);
                    self.push_val(TaintTypeOrFormula::TaintedVar(*global_taint));
                } else {
                    // if the global hasn't been previously set with SetGlobal, then it is a constant.
                    println!("global is {:?}. pushing to taint stack", TaintType::Constant(wasm::ast::Val::I32(7777)));
                    self.push_val(TaintType::Constant(wasm::ast::Val::I32(7777)).into());
                }
            },
            wasm::ast::highlevel::GlobalOp::SetGlobal => {
                let taint_elem = self.pop_val();
                //self.2.insert(global_idx, taint_elem);
                println!("setting global. stack var has taint: {:?}", taint_elem);
                // TODO: TaintTypeOrFormula .into() TaintType here..
                // want to preserve formula
                (self.2).0.insert(global_idx, taint_elem.into());
            },
        }
    }

    //pub fn call_instr(&mut self, ty: &InstrType, instr: wasm::ast::highlevel::Instr, target_idx: Idx<wasm::ast::highlevel::Function>) {
    pub fn call_instr(&mut self, ty: &InstrType, source_idx: Idx<wasm::ast::highlevel::Function>, target_idx: Idx<wasm::ast::highlevel::Function>, call_pos: InstrPosition) {
        println!("TaintStack.call_instr source_idx: {:?}  target_idx: {:?}  ty: {:?}  ", source_idx, target_idx, ty);
        if ty.inputs.len() > 0 {
            println!("input params for called func will have taint stack:");
            self.print_taint_stack();
        } else {
            println!("called function has no input params...");
        }

        // TODO: propagate call param taints to called function.

        // call_graph is at self.4
        //call_graph.push((fidx.0, target_func_idx.0));
        let mut input_taint_vec = Vec::new();

        for &_input_ty in ty.inputs.iter() {
            let taint_type_or_formula = self.pop_val();
            // TODO: TaintTypeOrFormula into TaintType here..
            // want to preserve formula
            input_taint_vec.push(TaintType::from(taint_type_or_formula));
        }

        let call_to_func = CallToFunc {
            input_taints: input_taint_vec,
            source_idx: source_idx.0,
            target_idx: target_idx.0,
            call_pos: call_pos,
        };
        self.4.push(call_to_func);

        //println!("Calling to function")

        // TODO: look up getCallDataSize instead of using index 3
        if target_idx.0 == 3 {
            //self.push_val(TaintType::InputSize(OpChain(Vec::new())));
            //self.push_val(TaintType::InputSize.into());
            println!("TaintStack pushing InputSizeFormula...");
            self.push_val(TaintTypeOrFormula::InputSizeFormula(Vec::new()));

            // TODO:
            // dont use TaintType::InputSize. instead use TaintStackElement::InputSizeFormula

            // TaintType::InputSize(OpChain(Vec::new()))
            // TaintStackElement::InputSizeFormula()
            // pub struct OpAndInputs (NumericOp, Vec<TaintType>);
            // pub struct InputSizeFormula (Vec<OpAndInputs>);

        } else if target_idx.0 == 2 {
            if ty.results.len() == 0 {
                // calling callDataCopy
                // callDataCopy will taint memory segment with tainttype `inputVal`
                println!("Call to callDataCopy.  no results");
            } else {
                panic!("Call to callDataCopy and have result length?!");
            }
        } else {
            // TODO: get taint from other functions
            // this will be done using taint IO flow maps
            for &_result_ty in ty.results.iter() {
                self.push_val(TaintType::Undetermined.into());
            }
        }
    }

    pub fn const_instr(&mut self, ty: &InstrType, val: wasm::ast::Val) {
        println!("TaintStack.const_isntr val: {:?}", val);
        let mut result_taint = TaintType::Constant(val);
        self.push_val(result_taint.into());
    }

    pub fn return_instr(&mut self, ty: &InstrType, source_idx: Idx<wasm::ast::highlevel::Function>, iidx: InstrPosition) {

        if ty.results.len() == 0 {
            println!("TaintStack.return_instr.  iidx: {:?}.  no return vals.", iidx);
        }
        else if ty.results.len() == 1 {
            let return_val_stack_el = self.pop_val();
            println!("TaintStack.return_instr.  iidx: {:?} return_val_stack_el: {:?}", iidx, return_val_stack_el);
        }
        else {
            panic!("return cant have more than 1 result");
        }

        // nothing needs to be pushed onto the stack.
        // also anything else on the type stack will be dropped anyway.

        // todos from taint_io.rs:
        // TODO: save location of return with the taint set
        // TODO: handle when func has multiple branches/returns
        // TODO: track taint flow through memory
    }

    pub fn numeric_instr(&mut self, op: wasm::ast::highlevel::NumericOp, ty: &InstrType) {
        println!("TaintStack.numeric_instr.. op: {:?}   inputs.len(): {:?}", op, ty.inputs.len());

        if ty.inputs.len() == 1 {
            // TODO: TaintTypeOrFormula .into() TaintType here..
            // want to preserve formula
            let taint_elem_1 = self.pop_val();
            println!("TaintStack.numeric_instr inputs: a={:?}", taint_elem_1);
            match taint_elem_1 {
                TaintTypeOrFormula::TaintedVar(TaintType::Constant(val1)) => {
                    println!("yay got constant!");
                    panic!("to be implemented.");
                },
                TaintTypeOrFormula::InputSizeFormula(op_chain) => {
                    println!("yay got unary op on InputSize!!");
                    let mut new_op_chain: Vec<OpAndInputs> = Vec::new();
                    for el in op_chain {
                        new_op_chain.push(el.clone());
                    }
                    let op_inputs = vec![TaintType::InputSize];
                    new_op_chain.push(OpAndInputs(op, op_inputs));

                    if ty.results.len() == 1 {
                        self.push_val(TaintTypeOrFormula::InputSizeFormula(new_op_chain));
                        return;
                    } else {
                        panic!("numeric unary op with results.len != 1!! is possible?");
                    }
                }
                TaintTypeOrFormula::TaintedVar(TaintType::InputSize) => {
                    println!("ERROR. This should be an inputsize formula, not Inputsize!!");
                    self.push_val(taint_elem_1);
                    self.instr(ty);
                    return;
                }
                _ => {
                    self.push_val(taint_elem_1);
                    self.instr(ty);
                    return;
                }
            }
        } else if ty.inputs.len() == 2 {
            // TODO: TaintTypeOrFormula .into() TaintType here..
            // want to preserve formula
            //let taint_elem_1 = TaintType::from(self.pop_val());
            //let taint_elem_2 = TaintType::from(self.pop_val());
            let mut taint_elem_1 = self.pop_val();
            let mut taint_elem_2 = self.pop_val();
            println!("inputs: a={:?}   b={:?}", taint_elem_1, taint_elem_2);
            match (&taint_elem_1, &taint_elem_2) {
                //(TaintType::Constant(val1), TaintType::Constant(val2)) => {
                (TaintTypeOrFormula::TaintedVar(TaintType::Constant(val1)),
                TaintTypeOrFormula::TaintedVar(TaintType::Constant(val2))) => {

                    println!("yay got two constants!");
                    //panic!("to be implemented.");
                    let result = do_binary_op(op, *val1, *val2);
                    self.push_val(TaintType::Constant(result).into());
                    return;
                },
                ( TaintTypeOrFormula::TaintedVar(TaintType::InputSize),
                  TaintTypeOrFormula::TaintedVar(TaintType::Constant(val1)) )
                |
                ( TaintTypeOrFormula::TaintedVar(TaintType::Constant(val1)),
                  TaintTypeOrFormula::TaintedVar(TaintType::InputSize) ) => {
                      println!("ERROR! This should be an input size formula, not taintype::inputsize!!");
                      self.push_val(taint_elem_2);
                      self.push_val(taint_elem_1);
                      self.instr(ty);
                      return;
                },

                // TODO: need to handle from TaintType::InputSize to new formula...
                ( TaintTypeOrFormula::InputSizeFormula(op_chain),
                  TaintTypeOrFormula::TaintedVar(TaintType::Constant(val1)) )
                |
                ( TaintTypeOrFormula::TaintedVar(TaintType::Constant(val1)),
                  TaintTypeOrFormula::InputSizeFormula(op_chain) ) => {
                    println!("yay got inputSize and a constant!");
                    //panic!("to be implemented.");
                    //let result = do_binary_op(op, val1, val2);
                    let mut new_op_chain: Vec<OpAndInputs> = Vec::new();
                    for el in op_chain {
                        new_op_chain.push(el.clone());
                    }

                    if ty.results.len() == 1 {
                        //self.push_val(TaintTypeOrFormula::InputSizeFormula(Vec::new()));
                        self.push_val(TaintTypeOrFormula::InputSizeFormula(new_op_chain));
                        return;
                    } else {
                        panic!("numeric op with results.len != 1!! to implement?");
                    }


                    // TODO:
                    // dont use TaintType::InputSize. instead use TaintStackElement::InputSizeFormula

                    // TaintType::InputSize(OpChain(Vec::new()))
                    // TaintStackElement::InputSizeFormula()
                    // pub struct OpAndInputs (NumericOp, Vec<TaintType>);
                    // pub struct InputSizeFormula (Vec<OpAndInputs>);
                },
                (_,_) => {
                    self.push_val(taint_elem_2);
                    self.push_val(taint_elem_1);
                    self.instr(ty);
                    return;
                }
            }
        }

        panic!("unimplemented.");
    }

    /// convenience, pops and validates input_tys, then pushes the result_tys
    pub fn instr(&mut self, ty: &InstrType) {
        println!("TaintStack.instr..");

        // *.const ops have ty.inputs.len() == 0
        if ty.inputs.len() == 0 && ty.results.len() == 1 {
            panic!("Got something here that wasnt handled by const!!");
            //self.const_instr(ty);
        } else {
            let mut result_taint: Option<TaintType> = None;
            // TODO: use reducer instead of iter here??
            for &_input_ty in ty.inputs.iter().rev() {
                let taint_elem = self.pop_val();
                match result_taint {
                    Some(val) => {
                        // TODO: TaintTypeOrFormula .into() TaintType here..
                        // want to preserve formula
                        result_taint = Some(taint_variable(result_taint.unwrap(), taint_elem.into()));
                    }
                    None => {
                        // TODO: TaintTypeOrFormula .into() TaintType here..
                        // want to preserve formula
                        result_taint = Some(taint_elem.into());
                    }
                }

                //self.pop_val();
                //assert_eq!(input_ty, self.pop_val(), "instruction expected input type, but stack top was");
            }
            for &_result_ty in ty.results.iter() {
                // TODO: fix for call_indirect...
                self.push_val(result_taint.unwrap().into());
            }
        }
    }

    pub fn begin(&mut self, block_ty: BlockType) {
        println!("TaintStack.begin..");
        self.0.push(BlockBegin(block_ty))
    }

    /// implicitly pops all types from the stack until the last block begin
    /// pushes that blocks result type on the stack
    /// returns the BlockType of that last block, or None if the last block was the whole function
    pub fn end(&mut self) -> Option<BlockType> {
        println!("taint stack end..");
        loop {
            match self.0.pop() {
                None => {
                    println!("taint stack end but nothing on stack...");
                    return None;
                    //panic!("could not end block, no block begin was found on type stack")
                },
                Some(_ty) => {}
                /*
                Some(BlockBegin(block_ty)) => {
                    // NOTE there is no validation that the stack is correct at the end of a block
                    // it is unclear to me how it exactly works with, e.g., br/return + drops
                    if let BlockType(Some(_ty)) = block_ty {
                        //self.push_val(ty);
                        // TODO: this is wrong
                        //self.push_val(TaintStackElement(InputVal));
                        //self.push_val(BlockType);
                    }
                    return Some(block_ty);
                }
                Some(FunctionBegin) => return None
                */
            }
        }
    }

    pub fn else_(&mut self) {
        println!("TaintStack.else...");
        // reuse code from end...
        let block_ty = self.end().expect("else cannot end a function");
        // but undo pushing of block result (this will be done by the "real" end)
        if let BlockType(Some(_ty)) = block_ty {
            //assert_eq!(ty, self.pop_val());
        }
        self.begin(block_ty);
    }

// TODO see add_hooks/mod.rs
//    pub fn unreachable(&mut self) {
//        self.0.push(TypeStackElement::Unreachable)
//    }
}
