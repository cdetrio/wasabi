use wasm::ast::{BlockType, InstrType, Idx};
//use wasm::ast::highlevel::{GlobalOp::*, LocalOp::*};
use self::TaintIOStackElement::*;
use wasm::ast::highlevel::Instr;

use std::collections::HashMap;
use std::collections::HashSet;

use std::hash::Hash;

use super::block_stack;

//use std::iter::FromIterator;

//use itertools::Itertools;

//use std::rc::Rc;



#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum InputVar {
    LocalVar(Idx<wasm::ast::Local>),
    GlobalVar(Idx<wasm::ast::highlevel::Global>),
    Constant,
    MemoryVal, // a value loaded/derived from memory
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FlowVarType {
    ReturnVal,
    BrIfConditionalVal,
}


#[derive(Debug)]
pub struct VarTaintSet(Vec<InputVar>);


#[derive(Debug)]
pub struct VarTaintRecord {
   pub flow_var: FlowVarType,
   pub var_position: InstrPosition,
   pub in_flow_set: Vec<InputVar>,

    
    // pub instr_ TODO: decide where to save br_if position and loop target
}


impl VarTaintRecord {
    fn new(flow_var_type: FlowVarType) -> VarTaintRecord {
        VarTaintRecord {
            in_flow_set: Vec::new(),
            flow_var: flow_var_type,
            var_position: Idx::from(0),
        }
    }

    /*
    fn new_br_if() -> VarTaintSet {
        VarTaintSet {
            in_flow_set: Vec::new(),
            flow_var: FlowVar::BrIfConditionalVal,
        }
    }
    */
}


type InstrPosition = Idx<Instr>;

/*

br_conditional_var: TaintType,
br_position: InstrPosition,
br_target: block_stack::BranchTarget,

#[allow(non_snake_case)]
pub fn VarTaintSet(v: Vec<InputVar>) -> VarTaintSet {
    VarTaintSet(v)
}
*/


impl VarTaintSet {
    fn new() -> VarTaintSet {
        VarTaintSet(Vec::new())
    }
    
    fn new_return_val(self) -> VarTaintSet {
        //self.new()
        VarTaintSet::new()
    }

    fn add(&mut self, elem: InputVar) {
        //self.0.insert(elem);
        self.0.push(elem);
    }

    fn insert(&mut self, elem: InputVar) {
        //self.0.insert(elem);
    }

    fn to_hashset(&mut self) -> &Vec<InputVar> {
        &self.0
    }

    fn to_vec(self) -> Vec<InputVar> {
        self.0
    }
    
    fn to_vec_mut(&mut self) -> &Vec<InputVar> {
        &self.0
    }
    
    fn to_vec_owned(self) -> Vec<InputVar> {
        self.0.to_owned()
    }

    fn clone_vector(&self) -> Vec<InputVar> {
        //let mut new_vec: Vec<InputVar> = Vec::new();
        let new_vec = 
        self.0.iter().map(|&val| {
                let new_var = match val {
                    InputVar::LocalVar(l_idx) => {
                        InputVar::LocalVar(l_idx.clone())
                    },
                    InputVar::GlobalVar(g_idx) => {
                        InputVar::GlobalVar(g_idx.clone())
                    },
                    InputVar::Constant => InputVar::Constant,
                    InputVar::MemoryVal => InputVar::MemoryVal,
                };
                new_var
         }).collect();
         
         new_vec
    }

}




fn clone_input_vector(orig: &Vec<InputVar>) -> Vec<InputVar> {
    let new_vec = orig.iter().map(|&val| {
            let new_var = match val {
                InputVar::LocalVar(l_idx) => {
                    InputVar::LocalVar(l_idx.clone())
                },
                InputVar::GlobalVar(g_idx) => {
                    InputVar::GlobalVar(g_idx.clone())
                },
                InputVar::Constant => InputVar::Constant,
                InputVar::MemoryVal => InputVar::MemoryVal,
            };
            new_var
     }).collect();

    new_vec
}






#[derive(Debug)]
pub enum TaintIOStackElement {
    VarTaintSet(Vec<InputVar>),
    BlockBegin(BlockType),
    FunctionBegin,
// TODO see add_hooks/mod.rs
//    Unreachable,
}




impl From<VarTaintSet> for TaintIOStackElement {
    fn from(v: VarTaintSet) -> TaintIOStackElement {
        TaintIOStackElement::VarTaintSet(v.to_vec())
    }
}





impl From<TaintIOStackElement> for VarTaintSet {
    fn from(tse:TaintIOStackElement) -> VarTaintSet {
        //let vt: VarTaintSet = v;
        match tse {
            TaintIOStackElement::VarTaintSet(v) => {
                //*self.into()
                VarTaintSet(v)
            },
            BlockBegin(_block_ty) => {
                //HashSet::new()
                VarTaintSet::new()
            }
            FunctionBegin => {
                //HashSet::new()
                VarTaintSet::new()
            }
        }
       
    }
}


impl TaintIOStackElement {

    pub fn vec_len(&self) -> usize {
        match self {
            TaintIOStackElement::VarTaintSet(v) => {
                v.len()
            },
            BlockBegin(_block_ty) => {
                0
            }
            FunctionBegin => {
                0
            }
        }
    }


    pub fn copy_data(self, b: &mut Vec<InputVar>) {
        match self {
            TaintIOStackElement::VarTaintSet(v) => {
                //*self.into()
                for data in v {
                    b.push(data);
                }
            },
            BlockBegin(_block_ty) => {
                //HashSet::new()
                
            }
            FunctionBegin => {
                //HashSet::new()
                
            }
        }
    }
 
}





impl From<&mut TaintIOStackElement> for VarTaintSet {
    fn from(tse: &mut TaintIOStackElement) -> VarTaintSet {
        match tse {
            TaintIOStackElement::VarTaintSet(v) => {
                VarTaintSet(v.to_vec())
            },
            BlockBegin(_block_ty) => {
                VarTaintSet::new()
            }
            FunctionBegin => {
                VarTaintSet::new()
            }
        }
       
    }
}






#[derive(Debug)]
pub struct TaintIOStack<'lt> (Vec<TaintIOStackElement>, LocalTaintIo, GlobalTaintIo, &'lt  mut ModuleFuncTaintFlowMaps);



#[derive(Debug)]
pub struct FunctionInputOutputTaints (Vec<Vec<InputVar>>);
/* ****
function input -> output taint map is a 2d list

input_params = [l_1, l_1, l_3]
global_vars = [g_1, g_2, g_3, g_4]
//input_vars = {l_1, l_1, l_3, g_1, g_2, g_3, g_4}

result vars = [i32, i32] // two result vars: 0, 1

result_vars_tainted_by[0] = [l_0, l_1] // the first result var is tainted by inputs 0 and 1
result_vars_tainted_by[1] = [l_1, l_2, g_3] // the second result var is tainted by inputs 1 and 2, and global var 3

*/

impl<'lt> FunctionInputOutputTaints {
    pub fn new() -> Self {
        println!("FunctionInputOutputTaints.new!");
        let function_taints = vec![vec![]];
        FunctionInputOutputTaints(function_taints)
    }
}


type LocalTaintIo = HashMap<Idx<wasm::ast::Local>, VarTaintSet>;


type GlobalTaintIo = HashMap<Idx<wasm::ast::highlevel::Global>, VarTaintSet>;



fn dedup<T: Eq + Hash + Copy>(v: &mut Vec<T>) { // note the Copy constraint
    let mut uniques = HashSet::new();
    v.retain(|e| uniques.insert(*e));
}



fn taint_stack_var(a: &Vec<InputVar>, b: &Vec<InputVar>) -> Vec<InputVar> {
    println!("taint io, taint_stack_var.  a:{:?}    b:{:?}", a, b);
    let mut a_mut = a.clone();
    let mut b_mut = b.clone();
    a_mut.append(&mut b_mut);
    dedup(&mut a_mut);
    a_mut
}

// TODO: don't just do the flow map from function inputs to return vals.  Also include the loop-targett9ing br_if conditional vars in the flow map.

impl<'lt> TaintIOStack<'lt> {
    pub fn new(module_func_taint_flow_maps: &'lt mut ModuleFuncTaintFlowMaps) -> Self {
        println!("TaintIOStack.new!");
        let global_taint_io = HashMap::new();
        let local_taint_io = HashMap::new();
        //TaintStack(vec![FunctionBegin], local_taints, global_taint_map)
        TaintIOStack(vec![TaintIOStackElement::FunctionBegin], local_taint_io, global_taint_io, module_func_taint_flow_maps)
    }

    //pub fn push_val(&mut self, ts: VarTaintSet) {
    pub fn push_val(&mut self, ts: TaintIOStackElement) {
        println!("TaintIOStack.push_val {:?}", ts);
        // add to vector at top of stack...
        // vector should be unique list
        self.0.push(ts)
        //self.0.push(ty)
    }

    pub fn print_taint_stack(&mut self) {
        println!("taint io stack: {:?}", self.0);
        //println!("top of stack: {:?}", self.0.last().unwrap());
    }

    /// panics if stack is empty or if there was a block begin (and not a ValType)
    pub fn pop_val(&mut self) -> TaintIOStackElement {
        let popped_el = self.0.pop().unwrap();
        println!("TaintIOStack.pop_val {:?}", popped_el);
        popped_el
    }

    pub fn memory_store_instr(&mut self, op: wasm::ast::highlevel::StoreOp, memarg: wasm::ast::Memarg ) {
        // mem store ops pop two vals off the stack and push 0
        
        // TODO: track memory offset taints
        
        let _val_to_store = self.pop_val();
        let _i_offset = self.pop_val();
        // actual offset is i + memarg.offset
    }
    
    pub fn memory_load_instr(&mut self, op: wasm::ast::highlevel::LoadOp, memarg: wasm::ast::Memarg ) {
        // mem loads pop one val off the stack and push one
        let _i_offset = self.pop_val();
        // memory position is i + memarg.offset
        // TODO: get taint of memory position

        let mut stack_var = Vec::new();
        stack_var.push(InputVar::MemoryVal);
        println!("memory load. pushing to io taint stack {:?}", stack_var);
        self.push_val(VarTaintSet(stack_var).into());
    }


    pub fn local_instr(&mut self, op: wasm::ast::highlevel::LocalOp, local_idx: Idx<wasm::ast::Local>) {
        println!("TaintIOStack.local_instr  {:?}  {:?}", op, local_idx);

        match op {
            wasm::ast::highlevel::LocalOp::GetLocal => {
                if self.1.contains_key(&local_idx) {
                    // if local var has previously been set with SetLocal, then it is tainted
                    // so push taint set onto the taint io stack
                    let local_taint_set_ref: &mut VarTaintSet = self.1.get_mut(&local_idx).unwrap();
                    println!("local var has taint set: {:?}", local_taint_set_ref);

                    let new_vec = (*local_taint_set_ref).to_vec_mut().iter().map(|&val| {
                            let new_var = match val {
                                InputVar::LocalVar(l_idx) => {
                                    InputVar::LocalVar(l_idx.clone())
                                },
                                InputVar::GlobalVar(g_idx) => {
                                    InputVar::GlobalVar(g_idx.clone())
                                },
                                InputVar::Constant => InputVar::Constant,
                                InputVar::MemoryVal => InputVar::MemoryVal,
                            };
                            new_var
                     }).collect();
                     
                     println!("pushing copy of taint set onto stack: {:?}", new_vec);
                     self.push_val(VarTaintSet(new_vec).into());

                } else {
                    // if the local hasn't been previously set with SetLocal, then its taint set is itself
                    let mut stack_var = Vec::new();
                    stack_var.push(InputVar::LocalVar(local_idx));
                    println!("local var {:?}. pushing to io taint stack {:?}", local_idx, stack_var);
                    self.push_val(VarTaintSet(stack_var).into());
                }
            },
            wasm::ast::highlevel::LocalOp::SetLocal => {
                // taint local with taint set of stack element
                let l_var_new_taint = self.pop_val();
                println!("setting local. stack var has io taint: {:?}", l_var_new_taint);

                // dont need to check for existing taint set, will be overwritten
                self.1.insert(local_idx, l_var_new_taint.into());
            },
            wasm::ast::highlevel::LocalOp::TeeLocal => {
                let mut stack_elem_taint_ref = VarTaintSet::from(self.0.last_mut().unwrap());
                println!("teeing local. stack var has io taint: {:?}", stack_elem_taint_ref);
                // overwrite existing taint set

   
                let new_vec = (stack_elem_taint_ref).to_vec_mut().iter().map(|&val| {
                        let new_var = match val {
                            InputVar::LocalVar(l_idx) => {
                                InputVar::LocalVar(l_idx.clone())
                            },
                            InputVar::GlobalVar(g_idx) => {
                                InputVar::GlobalVar(g_idx.clone())
                            },
                            InputVar::Constant => InputVar::Constant,
                            InputVar::MemoryVal => InputVar::MemoryVal,
                        };
                        new_var
                 }).collect();
                 
                self.1.insert(local_idx, VarTaintSet(new_vec));

            }
        }
    }

    pub fn global_instr(&mut self, op: wasm::ast::highlevel::GlobalOp, global_idx: Idx<wasm::ast::highlevel::Global>) {
        println!("TaintIOStack.global_instr  {:?}  {:?}", op, global_idx);
        match op {
            wasm::ast::highlevel::GlobalOp::GetGlobal => {
                if self.2.contains_key(&global_idx) {
                    // if global var has previously been set with SetGlobal, then it is tainted
                    // so push taint set onto the taint io stack
                    let g_var_taint_ref = self.2.get_mut(&global_idx).unwrap();
                    println!("global var has taint set: {:?}", g_var_taint_ref);
                    //let global_taint_set: VarTaintSet = *g_var_taint_set;
                    
                    let mut g_var_taint_set: Vec<InputVar> = Vec::new();
                    for data in g_var_taint_ref.to_vec_mut() {
                        g_var_taint_set.push(*data);
                    }

                    //local_taint_set_ref
                    self.push_val(VarTaintSet(g_var_taint_set).into());
                    //self.push_val(global_taint_set.into());
                } else {
                    // if the global hasn't been previously set with SetGlobal, then its taint set is itself
                    let mut stack_var = Vec::new();
                    stack_var.push(InputVar::GlobalVar(global_idx));
                    println!("global var {:?}. pushing to taint stack {:?}", global_idx, stack_var);
                    self.push_val(VarTaintSet(stack_var).into());
                }
            },
            wasm::ast::highlevel::GlobalOp::SetGlobal => {
                // existing taint set is overwritten.
                let g_var_new_taint = self.pop_val();
                self.2.insert(global_idx, g_var_new_taint.into());
            },
        }
    }

    //pub fn call_instr(&mut self, ty: &InstrType, instr: wasm::ast::highlevel::Instr, target_idx: Idx<wasm::ast::highlevel::Function>) {
    pub fn call_instr(&mut self, ty: &InstrType, source_idx: Idx<wasm::ast::highlevel::Function>, target_idx: Idx<wasm::ast::highlevel::Function>) {
        println!("TaintIOStack.call_instr source_idx: {:?}  target_idx: {:?}  ty: {:?}  ", source_idx, target_idx, ty);
        if ty.inputs.len() > 0 {
            println!("input params for called func will have taint stack:");
            self.print_taint_stack();
        } else {
            println!("called function has no input params...");
        }


        // TODO: save to IO flow record, the flow stack elements just before a call.  this can be used to deduce the var taints passed to the call.
        // TODO: fix this.  for now just assuming that all result vars from a call are tainted by all inputs

        let mut call_input_set = Vec::new();

        for &input_ty in ty.inputs.iter() {
            let input_elem = self.pop_val();
            //call_input_set = taint_stack_var(call_input_set, *input_elem.to_taint_set().to_vec());
            call_input_set = taint_stack_var(&call_input_set, VarTaintSet::from(input_elem).to_vec_mut());
        }

        for &_result_ty in ty.results.iter() {
            let call_input_set_copy = clone_input_vector(&call_input_set);
            self.push_val(VarTaintSet(call_input_set_copy).into());
        }

    }
    
    pub fn return_instr(&mut self, ty: &InstrType, source_idx: Idx<wasm::ast::highlevel::Function>, iidx: InstrPosition) {
        //  taint_io_stack.return_instr(&InstrType::new(&[], &function.type_.results), fidx, target_func_idx);
        
        // with a return, we have enough info to save the final taint flow graph for the function.
        // save in the module taint struct, so other functions can lookup this graph when they call this func
        
        // wasm funcs only return one value, so the taint flow graph is simply one vector
        // the vector is a list of the input params and globals that taint the return val
        
        // final taint flow graph type is VarTaintSet
        
        println!("return_instr.  iidx: {:?}  taint io stack: {:?}", iidx, self.print_taint_stack());

        let return_val_stack_el = self.pop_val();
        let return_val_taints = VarTaintSet::from(return_val_stack_el);
        let return_val_taints_copy = return_val_taints.clone_vector();

        if return_val_taints_copy.to_vec().len() > 0 {
            println!("got a return on function {:?}. saving return_val_taints: {:?}", source_idx, return_val_taints);
            self.3.save_func_return_taint(source_idx, return_val_taints, iidx);
        } else {
            if ty.results.len() > 0 {
                println!("ERROR?? function has return type but no taint found on the stack.");
            } else {
                println!("function {:?} has no return vals.", source_idx);
            }
        }


        // TODO: save location of return with the taint set
        // TODO: handle when func has multiple branches/returns
        // TODO: track taint flow through memory
    }

    // TODO: handle memory ops.  memory should be another input class in taint io maps (along with locals and globals)


    pub fn instr(&mut self, ty: &InstrType) {
        println!("TaintIOStack.instr..");
        let mut result_set = Vec::new();

        // TODO: use reducer instead of iter here??
        for &_input_ty in ty.inputs.iter().rev() {
            let stack_elem = self.pop_val();
            //let stack_elem_copy = clone_input_vector(&VarTaintSet::from(stack_elem).to_vec());
            if stack_elem.vec_len() == 0 {
                println!("skipping stack element with no taint (must be a constant).");
                continue;
            }
            println!("adding top stack elem: {:?} to result taint set.", stack_elem);
            result_set = taint_stack_var(&result_set, VarTaintSet::from(stack_elem).to_vec_mut());

        }
        
        println!("all inputs combined to result set: {:?}", result_set);
        
        if ty.inputs.len() == 0 && ty.results.len() == 1 {
            // op with no inputs and 1 stack result is a const
             if result_set.len() == 0 {
                 result_set.push(InputVar::Constant);
             } else {
                 panic!("ERROR.  result_set should be empty if no instruction inputs!!");
             }
        }

        for &_result_ty in ty.results.iter() {
            // what operations put more than one result on the stack?
            // indirect calls also should only return one value...
            let result_set_copy = clone_input_vector(&result_set);
            // TODO: fix for call_indirect...

            self.push_val(VarTaintSet(result_set_copy).into());
        }
    }


    pub fn add_loop_br_control(&mut self, fn_id: usize, br_position: InstrPosition, br_target: block_stack::BranchTarget) {
        // get current taint var...
        println!("TaintIOStack add_loop_br_control for fn_id {:?}.  br_position: {:?}   br_target: {:?}", fn_id, br_position, br_target);

        let mut stack_elem_taint_ref = VarTaintSet::from(self.0.last_mut().unwrap());
        println!("loop-targeting br_if. stack var has in flow: {:?}", stack_elem_taint_ref);

        self.3.save_func_brif_flow(fn_id, stack_elem_taint_ref, br_position);

        //println!("got a return on function {:?}. saving return_val_taints: {:?}", source_idx, return_val_taints);
        //self.3.save_func_return_taint(source_idx, return_val_taints);
        
        //br_to_loop: BrToLoop
        //let br_conditional_var = self.0.last().unwrap();
        //let br_conditional_taint_type = br_conditional_var.to_taint();
        //println!("br to loop conditional var has taint: {:?}", br_conditional_taint_type);
        /*
        let br_to_loop = BrToLoop {
            br_position: br_position,
            br_target: br_target,
            br_conditional_var: br_conditional_taint_type,
        };
        self.3.add_loop_br_control(fn_id, br_to_loop);
        */
    }



    pub fn begin(&mut self, block_ty: BlockType) {
        println!("TaintIOStack.begin..");
        self.0.push(BlockBegin(block_ty))
    }

    /// implicitly pops all types from the stack until the last block begin
    /// pushes that blocks result type on the stack
    /// returns the BlockType of that last block, or None if the last block was the whole function
    pub fn end(&mut self) -> Option<BlockType> {
        println!("TaintIOStack end..");
        loop {
            match self.0.pop() {
                None => {
                    println!("taint stack end but nothing on stack...");
                    return None;
                    //panic!("could not end block, no block begin was found on type stack")
                },
                Some(_ty) => {}
            }
        }
    }

    pub fn else_(&mut self) {
        println!("TaintIOStack.else...");
        // reuse code from end...
        let block_ty = self.end().expect("else cannot end a function");
        // but undo pushing of block result (this will be done by the "real" end)
        if let BlockType(Some(_ty)) = block_ty {
            //assert_eq!(ty, self.pop_val());
            self.pop_val();
        }
        self.begin(block_ty);
    }

}





#[derive(Debug)]
//pub struct ModuleFuncTaintFlowMaps (HashMap<Idx<wasm::ast::highlevel::Function>, Vec<VarTaintSet>>);
pub struct ModuleFuncTaintFlowMaps (HashMap<Idx<wasm::ast::highlevel::Function>, Vec<VarTaintRecord>>);

// map function id to VarTaintSet for the function return val

impl<'lt> ModuleFuncTaintFlowMaps {
    pub fn new() -> Self {
        println!("ModuleFuncTaintFlowMaps.new!");
        let function_taints = HashMap::new();
        ModuleFuncTaintFlowMaps(function_taints)
    }
    
    pub fn init_fidx_vec(&mut self, fidx: Idx<wasm::ast::highlevel::Function>) {
        if !self.0.contains_key(&fidx) {
            self.0.insert(fidx, Vec::new());
        }
    }

    pub fn save_func_return_taint(&mut self, fidx: Idx<wasm::ast::highlevel::Function>, return_taint: VarTaintSet, iidx: InstrPosition) {
        //TODO: also save location of return, to distinguish when multiple returns
        self.init_fidx_vec(fidx);
        //let fidx_vec = self.0.get(&fidx);
        let fidx_vec = self.0.get_mut(&fidx).unwrap();
        println!("saving return_taint for function {:?}.  iidx: {:?}  prior existing return_taints: {:?}", fidx,iidx,  fidx_vec.len());
        let return_taint_record = VarTaintRecord {
                    in_flow_set: return_taint.to_vec(),
                    flow_var: FlowVarType::ReturnVal,
                    var_position: iidx,
                };
        fidx_vec.push(return_taint_record);
        println!("return_taints after save {:?}", fidx_vec.len());
    }

    pub fn save_func_brif_flow(&mut self, fnid: usize, brif_in_flow: VarTaintSet, iidx: InstrPosition) {
        //TODO: also save location of return, to distinguish when multiple returns
        let fidx = Idx::<wasm::ast::highlevel::Function>::from(fnid);
        self.init_fidx_vec(fidx);
        //let fidx_vec = self.0.get(&fidx);
        let fidx_vec = self.0.get_mut(&fidx).unwrap();
        println!("saving brif_taint for function {:?}. prior existing flow records: {:?}", fidx, fidx_vec.len());
        let brif_flow_record = VarTaintRecord {
                    in_flow_set: brif_in_flow.to_vec(),
                    flow_var: FlowVarType::BrIfConditionalVal,
                    var_position: iidx,
                };
        fidx_vec.push(brif_flow_record);
        println!("brif_in_flows after save {:?}", fidx_vec.len());
    }

    /*
    pub fn set_func_return_taint(&mut self, fidx: Idx<wasm::ast::highlevel::Function>, return_taint: VarTaintSet) {
        println!("saving function {:?}  return taint: {:?}", fidx, return_taint);
        self.0.insert(fidx, return_taint);
    }
    */

    pub fn print_all_func_return_taints(&mut self) {

        println!("");
        println!("--------------------------------------------");
        println!("function IO flow maps. for each function, which input params and globals taint the return var.");
        println!("The data structure is currently named \"Taint Input-Output Flow Map\", because it represents a map of the data flow from a function's inputs to its output return val.");
        println!("The initial motivation for the IO Flow Map is to make it easy to propagate taints in main() through function calls and back.\n");

        let mut key_nums = Vec::new();
  
        for key in self.0.keys() {
            //println!("key: {:?}", key.0);
            key_nums.push(key.0);
        }
        
        key_nums.sort();
        
        for key in key_nums {
            //let idx = Idx { key, wasm::ast::highlevel::Function };
            //let fidx_typed: Idx<wasm::ast::highlevel::Function> = Idx::<wasm::ast::highlevel::Function> { 0: key, 1: std::marker::PhantomData, };
            let fidx_typed = Idx::<wasm::ast::highlevel::Function>::from(key);
            let flow_records_vec = self.0.get(&fidx_typed).unwrap();
            for flow_var in flow_records_vec {
                //flow_records_vec
                println!("function {:?} has IO flow var: {:?}", key, flow_var);
            }
            
        }

    }

    pub fn get_flow_maps(&mut self, fidx: Idx<wasm::ast::highlevel::Function>) -> Option<&Vec<VarTaintRecord>> {
        match self.0.get(&fidx) {
            None => None,
            Some(return_flow_map) => Some(return_flow_map),
        }

    }

    /*
    pub fn add_function_taints(&mut self, func_idx: Idx<wasm::ast::highlevel::Function>, func_io_taints: FunctionInputOutputTaints) {
        self.0.insert(func_idx, func_io_taints);
    }
    */
    /*
    pub fn print_function_taints(&mut self) {
        println!("module function taints: {:?}", self.0)
    }
    */
}




