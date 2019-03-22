use parking_lot::RwLock;
//use rayon::prelude::*;
use serde_json;
//use wasm::ast::{BlockType, FunctionType, Idx, InstrType, Mutability, Val, ValType::*};
use wasm::ast::{BlockType, Idx, InstrType, Mutability, Val, ValType::*};
use wasm::ast::highlevel::{Function, GlobalOp::*, Instr, Instr::*, LocalOp::*, Module};

use crate::config::{EnabledHooks, HighLevelHook};

use self::block_stack::{BlockStack, BlockStackElement};
use self::convert_i64::convert_i64_instr;
use self::duplicate_stack::*;
use self::hook_map::HookMap;
use self::static_info::*;
use self::type_stack::TypeStack;

use self::taint_stack::TaintStack;
use self::taint_stack::GlobalTaintMap;
//use self::taint_stack::LoopControllers;
use self::taint_stack::ModFuncLoopControls;
use self::taint_stack::CallGraph;
//use self::taint_stack::BrToLoop;
use self::taint_stack::TaintType;


use self::taint_io::TaintIOStack;
use self::taint_io::ModuleFuncTaintFlowMaps;
use self::taint_io::VarTaintSet;

mod convert_i64;
mod static_info;
mod block_stack;
mod type_stack;
mod taint_stack;
mod taint_io;
mod hook_map;
mod duplicate_stack;

//type SimpleCallGraph = Vec<(usize, usize)>;

/// instruments every instruction in Jalangi-style with a callback that takes inputs, outputs, and
/// other relevant information.
pub fn add_hooks(module: &mut Module, enabled_hooks: &EnabledHooks) -> Option<String> {
    // make sure table is exported, needed for Wasabi runtime to resolve table indices to function indices.
    for table in &mut module.tables {
        if table.export.is_empty() {
            table.export.push("__wasabi_table".into());
        }
    }
    // FIXME is this a valid workaround for wrong Firefox exported function .name property?
//    if let Some(function) = module.functions.first_mut() {
//        if function.export.is_empty() {
//            function.export.push("__wasabi_first_function".into());
//        }
//    }

    // NOTE must be after exporting table and function, so that their export names are in the static info object
    let module_info: ModuleInfo = (&*module).into();
    let module_info = RwLock::new(module_info);
    let hooks = HookMap::new(&module);

    let mut global_taint_map = GlobalTaintMap::new();

    let mut mod_func_loop_controls = ModFuncLoopControls::new();
 
    let mut module_func_taint_flow_maps = ModuleFuncTaintFlowMaps::new();
    // module_func_taint_flow_maps.set_func_return_taint(fid, return_var_taint_set)

    let mut call_graph = CallGraph::new();
    let mut simple_call_graph: Vec<(usize, usize)> = Vec::new();

    // let mut module_function_taints = ModuleFunctionTaints::new();

    // add global for start, set to false on the first execution of the start function
    let start_not_executed_global = module.add_global(I32, Mutability::Mut, vec![Const(Val::I32(1)), End]);

    //module.functions.par_iter_mut().enumerate().for_each(&|(fidx, function): (usize, &mut Function)| {
    module.functions.iter_mut().enumerate().for_each(&mut |(fidx, function): (usize, &mut Function)| {
        let fidx = fidx.into();
        // only instrument non-imported functions
        if function.code.is_none() {
            return;
        }

        let mut doing_main = false;
        let fidx_typed: Idx<Function> = fidx;
        println!("\n--------------------------------");
        println!("doing function... fidx:{:?}", fidx_typed.0);
        if fidx_typed.0 == 7 {
            println!("doing main function!!");
            doing_main = true;
            module_func_taint_flow_maps.print_all_func_return_taints();
        }

        // move body out of function, so that function is not borrowed during iteration over the original body
        let original_body = {
            let dummy_body = Vec::new();
            ::std::mem::replace(&mut function.code.as_mut().expect("internal error: function code should exist, see check above").body, dummy_body)
        };

        // allocate new instrumented body (i.e., do not modify in-place), since there are too many insertions anyway
        // there are at least 3 new instructions per original one (2 const for location + 1 hook call)
        //let mut instrumented_body = Vec::with_capacity(4 * original_body.len());

        // for branch target resolution (i.e., relative labels -> instruction locations)
        let mut block_stack = BlockStack::new(&original_body);
        // for drop/select monomorphization (cannot determine their input types only from instruction, but need this additional type information)
        let mut type_stack = TypeStack::new();

        let mut taint_stack = TaintStack::new(&mut global_taint_map, &mut mod_func_loop_controls, &mut call_graph);
        let mut taint_io_stack = TaintIOStack::new(&mut module_func_taint_flow_maps);


        // two types of taint analysis: taint_stack and taint_io_stack
        // taint_stack is more concrete, taint_io_stack is more symbolic
        // taint_stack tags stack slots as being either Constants (i.e. derived only from constants in the wasm code and module globals)
        // or inputSize (influenced by input size / getCallDataSize)
        // or inputVal (worst case if we want to do upper-bound metering)
        // additionally some stack slots may be Undetermined (if we pass over a call to a function we know nothing about, then the return val is tagged as undetermined)
        
        // taint_io_stack is more symbolic because we don't tag stack slots as being a particular taint class {Constant, inputSize, inputVal}
        // instead we tag stack slots by their "taint set" - the set of all inputs that influence a stack slot (stack slots and locals/globals)
        // then once we reach the function return, we know the taint set of the return value
        // we can use the taint set of the return value during the concrete analysis, when we encounter a
        // call to some function, we have the taint classes of the input params being passed in the call
        // e.g. (call $f7  $p0=inputVal $p1=inputVal $p2=Constant $p3=inputSize), and we know the current taint classes of the globals
        // we plug the taint classes into the function's taint flow map. so if the flow map is
        // returnVal = {$p2, $p3}  then we know the return val is of type (Cosntant * inputSize) = inputSize
        // now if this return val is used as a loop branching condition, then we've deduced that the runtime
        // is proportional to input size.



        // execute start hook before anything else
        if module_info.read().start == Some(fidx) {

        }

        // remember implicit return for instrumentation: add "synthetic" return hook call to last end
        let implicit_return = !original_body.ends_with(&[Return, End]);
        println!("function idx: {:?}  implicit_return {:?}", fidx, implicit_return);

        let mut unreachable = 0;

        for (iidx, instr) in original_body.into_iter().enumerate() {
            // FIXME super hacky: do not instrument dead code, since my type checking cannot handle
            // the unconstrained return types of return, br, br_table, unreachable and then
            // type_stack.pop_val() blows up because I cannot produce the right types "out of thin
            // air".
            // TODO integrate the "Unreachable" type into type_stack, remove this integer
            // "unreachable depth" abomination.
            if unreachable > 0 {
                match instr {
                    Block(_) | Loop(_) | If(_) => unreachable += 1,
                    End => unreachable -= 1,
                    _ => {}
                };
                if unreachable > 0 {
                    //instrumented_body.push(instr.clone());
                    continue;
                }
            }

//            println!("{:?}:{:?}: {:?}", fidx.0, iidx, instr);

            let iidx: Idx<Instr> = iidx.into();
            let location = (fidx.to_const(), iidx.to_const());

            println!("doing instr: {:?}", instr);
            // doing instr: Const(I32(2147483647))

            /*
             * add calls to hooks, typical instructions inserted for (not necessarily in this order if that saves us a local or so):
             * 1. duplicate instruction inputs via temporary locals
             * 2. call original instruction (except for in a few cases, where the hook is inserted before)
             * 3. duplicate instruction results via temporary locals
             * 4. push instruction location (function + instr index)
             * 5. call hook
             */
            match instr {
                Nop => {
 
                },
                Unreachable => {
                    // hook must come before unreachable instruction, otherwise it prevents hook from being called

                    unreachable = 1;
                }


                /* Control Instructions: Blocks */

                Block(block_ty) => {
                    block_stack.begin_block(iidx);
                    type_stack.begin(block_ty);
                    taint_stack.begin(block_ty);
                    taint_io_stack.begin(block_ty);

                }
                Loop(block_ty) => {
                    if doing_main {
                        println!("found loop instruction. {:?}", iidx);
                    }

                    block_stack.begin_loop(iidx);
                    type_stack.begin(block_ty);
                    taint_stack.begin(block_ty);
                    taint_io_stack.begin(block_ty);

                }
                If(block_ty) => {
                    block_stack.begin_if(iidx);
                    type_stack.instr(&InstrType::new(&[I32], &[]));
                    type_stack.begin(block_ty);
                    
                    taint_stack.instr(&InstrType::new(&[I32], &[]));
                    taint_stack.begin(block_ty);
                    
                    taint_io_stack.instr(&InstrType::new(&[I32], &[]));
                    taint_io_stack.begin(block_ty);

                }
                Else => {
                    let if_block = block_stack.else_();
                    let begin_if = if let BlockStackElement::If { begin_if, .. } = if_block {
                        begin_if
                    } else {
                        unreachable!()
                    };

                    type_stack.else_();
                    taint_stack.else_();
                    taint_io_stack.else_();

                }
                End => {
                    let block = block_stack.end();
                    assert_eq!(iidx, block.end());
                    type_stack.end();
                    
                    //taint_stack.end();
                    //taint_io_stack.end();

                    if implicit_return {
                        if let BlockStackElement::Function { .. } = block {
                            //taint_stack.instr(&InstrType::new(&[], &function.type_.results));
                            //taint_io_stack.instr(&InstrType::new(&[], &function.type_.results));

                            // taint_stack return types are never stored, so no point in processing them.
                            //taint_stack.instr(&InstrType::new(&[], &function.type_.results));

                            println!("taint_io_stack.return_instr implicit return...");
                            taint_io_stack.return_instr(&InstrType::new(&[], &function.type_.results), fidx, iidx);
                        }
                    }

                }

                /* Control Instructions: Branches/Breaks */
                // NOTE hooks must come before instr

                Br(target_label) => {
                    let br_target = block_stack.br_target(target_label);

                    if br_target.absolute_instr.0 < iidx.0 {
                        panic!("wow weird!! an infinite loop?");
                        // if its not an infinite loop, then there is a br_if prior to this br. in tihs case, the br_if prior is the loop controller
                    }

                    // stop instrumentation for this block: we do not need to look at dead code
                    unreachable += 1;
                }
                BrIf(target_label) => {
                    println!("br_if type_stack: {:?}", type_stack);

                    type_stack.instr(&InstrType::new(&[I32], &[]));

                    let br_target = block_stack.br_target(target_label);

                    //if doing_main {
                        // loops are the only targets you can branch to backwards
                        // so if the target position is less than the current position, we know the target is a loop
                        if br_target.absolute_instr.0 < iidx.0 {
                            println!("BrIf instr position: {:?}", iidx);
                            println!("BrIf br_target: {:?}", br_target);
                            println!("branching to a loop!!  in function idx: {:?}", fidx_typed.0);
                            taint_stack.print_taint_stack();

                            taint_stack.add_loop_br_control(fidx_typed.0, iidx, br_target.clone());

                            taint_io_stack.add_loop_br_control(fidx_typed.0, iidx, br_target.clone());
                        }
                        // when we're at a br_if that targets a loop, we need to inspect the conditional var
                        // the conditional var might be a simple counter, then the loop can be "unrolled"
                        // 
                    //}

                    taint_stack.instr(&InstrType::new(&[I32], &[]));
                    taint_io_stack.instr(&InstrType::new(&[I32], &[]));

                }
                BrTable(ref target_table, default_target) => {
                    // TODO: check if any targets in the table are loop instructions
                    println!("BrTable target_table: {:?}", target_table);
                    panic!("BrTable not implemented.");
                    
                    type_stack.instr(&InstrType::new(&[I32], &[]));
                    taint_stack.instr(&InstrType::new(&[I32], &[]));
                    taint_io_stack.instr(&InstrType::new(&[I32], &[]));

                }


                /* Control Instructions: Calls & Returns */

                Return => {
                    println!("mod.rs return...");
                    type_stack.instr(&InstrType::new(&[], &function.type_.results));
                    //taint_stack.instr(&InstrType::new(&[], &function.type_.results));
                    //taint_io_stack.instr(&InstrType::new(&[], &function.type_.results));

                    // taint_stack stack elements are just popped, nothing is done with them.
                    // dont need to call taint_stack.return_instr, but doesnt hurt either.
                    taint_stack.return_instr(&InstrType::new(&[], &function.type_.results), fidx, iidx);

                    // taint_io_stack saves the return vals to the IO flow map.
                    taint_io_stack.return_instr(&InstrType::new(&[], &function.type_.results), fidx, iidx);

                    unreachable = 1;
                }
                Call(target_func_idx) => {
                    let ref func_ty = module_info.read().functions[target_func_idx.0].type_;
                    type_stack.instr(&func_ty.into());

                    // build a call graph.  then check the call graph for a cycle
                    simple_call_graph.push((fidx.0, target_func_idx.0));

                    //println!("call to target_func_idx.0: {:?}", target_func_idx.0);
                    //println!("call to target func import: {:?}", module_info.read().functions[target_func_idx.0]);
                    // TODO: check if function is getCallDataSize using module_info.read().functions[target_func_idx.0].import
                    if target_func_idx.0 == 3 {
                        println!("CALLING TO getCallDataSize!!!");
                    }

                    if target_func_idx.0 == 2 {
                        println!("CALLING TO getCallDataCopy!!!");
                    }

                    // TODO: special handling taint stack for call()
                    //taint_stack.call_instr(&func_ty.into(), instr, target_func_idx);
                    taint_stack.call_instr(&func_ty.into(), fidx, target_func_idx, iidx);
                    taint_io_stack.call_instr(&func_ty.into(), fidx, target_func_idx);

                }
                CallIndirect(ref func_ty, _ /* table idx == 0 in WASM version 1 */) => {
                    type_stack.instr(&instr.to_type().unwrap());
                    taint_stack.instr(&instr.to_type().unwrap());
                    taint_io_stack.instr(&instr.to_type().unwrap());

                    panic!("not handling call_indirects yet...");
                }


                /* Parametric Instructions */

                Drop => {
                    let ty = type_stack.pop_val();
                    let _taint = taint_stack.pop_val();
                    let _taint_io = taint_io_stack.pop_val();

                }
                Select => {
                    assert_eq!(type_stack.pop_val(), I32, "select condition should be i32");
                    let ty = type_stack.pop_val();
                    assert_eq!(type_stack.pop_val(), ty, "select arguments should have same type");
                    type_stack.push_val(ty);

                    let taint_ty = taint_stack.pop_val();
                    //taint_stack.push_val(taint_ty);
                    taint_stack.push_val(TaintType::from(taint_ty));
                    

                    let taint_io_ty = taint_io_stack.pop_val();
                    taint_io_stack.push_val(taint_io_ty);

                }


                /* Variable Instructions */

                Local(op, local_idx) => {
                    let local_ty = function.local_type(local_idx);
                    println!("Local local_idx: {:?}", local_idx);

                    type_stack.instr(&op.to_type(local_ty));

                    //let local_taint_ty = function.local_taint(local_idx);
                    taint_stack.local_instr(op, local_idx);
                    //taint_stack.instr(&op.to_type(local_taint_ty));
                    
                    taint_io_stack.local_instr(op, local_idx);

                }
                Global(op, global_idx) => {
                    let global_ty = module_info.read().globals[global_idx.0];

                    type_stack.instr(&op.to_type(global_ty));

                    println!("Global global_idx: {:?}", global_idx);
                    taint_stack.global_instr(op, global_idx);
                    taint_io_stack.global_instr(op, global_idx);
                }


                /* Memory Instructions */

                MemorySize(_ /* memory idx == 0 in WASM version 1 */) => {
                    type_stack.instr(&instr.to_type().unwrap());

                }
                MemoryGrow(_ /* memory idx == 0 in WASM version 1 */) => {
                    type_stack.instr(&instr.to_type().unwrap());

                }

                /* rest are "grouped instructions", i.e., where many instructions can be handled in a similar manner */

                Load(op, memarg) => {
                    let ty = op.to_type();
                    type_stack.instr(&ty);

                    taint_stack.memory_load_instr(op, memarg);
                    taint_io_stack.memory_load_instr(op, memarg);

                }
                Store(op, memarg) => {
                    let ty = op.to_type();
                    type_stack.instr(&ty);

                    taint_stack.memory_store_instr(op, memarg);
                    taint_io_stack.memory_store_instr(op, memarg);

                }


                /* Numeric Instructions */

                Const(val) => {
                    type_stack.instr(&instr.to_type().unwrap());

                    println!("Const val {:?}:", val);

                    //taint_stack.instr(&instr.to_type().unwrap());
                    taint_stack.const_instr(&instr.to_type().unwrap(), val);
                    taint_io_stack.instr(&instr.to_type().unwrap());

                    //instrumented_body.push(instr.clone());

                }
                Numeric(op) => {
                    let ty = op.to_type();
                    type_stack.instr(&ty);

                    println!("Numeric op {:?} results length: {:?}", op, ty.results.len());
                    taint_stack.numeric_instr(op, &ty);
                    taint_io_stack.instr(&ty);

                }
            }

        } // done with all instructions in function


        if &function.type_.results.len() > &0 {
            println!("finished a function that has a return val. verify that we have return taint flow...");
            //let flow_map = module_func_taint_flow_maps.get_flow_map(fidx);
            match module_func_taint_flow_maps.get_flow_maps(fidx) {
                None => panic!("missing flow map!!!"),
                Some(elem) => println!("got flow maps: {:?}", elem)
            }
        }

        // finally, switch dummy body out against instrumented body
        //::std::mem::replace(&mut function.code.as_mut().unwrap().body, instrumented_body);
    }); // done with all functions
 

    // actually add the hooks to module and check that inserted Idx is the one on the Hook struct
    //let hooks = hooks.finish();
    //println!("generated {} low-level hooks", hooks.len());
//    let mut hook_list: Vec<(String, FunctionType)> = hooks.iter().map(|hook| (hook.wasm.import.as_ref().map(|opt| opt.1.clone()).unwrap(), hook.wasm.type_.clone())).collect();
//    hook_list.sort_by_key(|h| h.0.clone());
//    for hook in hook_list {
//        println!("{}: {:?}", hook.0, hook.1);
//    }
//    println!("{:?}", hook_list.iter().max_by_key(|hook| hook.1.params.len()));

    /*
    let mut js_hooks = Vec::new();
    for hook in hooks {
        js_hooks.push(hook.js);
        assert_eq!(hook.idx, module.functions.len().into(), "have other functions been inserted into the module since starting collection of hooks?");
        module.functions.push(hook.wasm);
    }

    Some(generate_js(module_info.into_inner(), &js_hooks))
    */

    println!("call graph:");
    println!("{:?}", simple_call_graph);
    //println!("{:?}\n", call_graph);
    call_graph.print();

    global_taint_map.print_global_taints();
    
    module_func_taint_flow_maps.print_all_func_return_taints();
    
    mod_func_loop_controls.print_all_loop_controls();

    Some("done".to_string())
}

/// convenience to hand (function/instr/local/global) indices to hooks
/// must be trait since inherent impl is disallowed by orphan rules for non-crate types (Idx<T>)
trait ToConst {
    fn to_const(self) -> Instr;
}

impl<T> ToConst for Idx<T> {
    fn to_const(self) -> Instr {
        Const(Val::I32(self.0 as i32))
    }
}

impl BlockStackElement {
    fn to_end_hook_args(&self, fidx: Idx<Function>) -> Vec<Instr> {
        match self {
            | BlockStackElement::Function { end } => vec![fidx.to_const(), end.to_const()],
            | BlockStackElement::Block { begin, end }
            | BlockStackElement::Loop { begin, end }
            | BlockStackElement::If { begin_if: begin, end, .. } => vec![fidx.to_const(), end.to_const(), begin.to_const()],
            | BlockStackElement::Else { begin_else, begin_if, end } => vec![fidx.to_const(), end.to_const(), begin_else.to_const(), begin_if.to_const()]
        }
    }
    fn end(&self) -> Idx<Instr> {
        use self::block_stack::BlockStackElement::*;
        match self {
            | Function { end }
            | Block { end, .. }
            | Loop { end, .. }
            | If { end, .. }
            | Else { end, .. } => *end
        }
    }
}

fn generate_js(module_info: ModuleInfo, hooks: &[String]) -> String {
    // FIXME somewhat hacky: just cat together long.js dependency, program-independent, and
    // program-dependent JavaScript into one big file.
    // * Alternative A: use webpack or other bundler, drawbacks:
    //    - users need to install another tool
    //    - needs to be run after every instrumentation
    // * Alternative B: compile Wasabi itself to WebAssembly, instrument at runtime
    format!(r#"/*
 * Generated by Wasabi. DO NOT EDIT.
 * Contains:
 *   - independent of program-to-instrument: long.js dependency, Wasabi loader and runtime
 *   - generated from program-to-instrument: static information and low-level hooks
 */

// long.js
{}

{}

Wasabi.module.info = {};

Wasabi.module.lowlevelHooks = {{
    {}
}};
"#,
            include_str!("../../../lib/long.js/long.js").lines().next().unwrap(),
            include_str!("../../../lib/runtime.js"),
            serde_json::to_string(&module_info).unwrap(),
            hooks.iter().flat_map(|s| s.split("\n")).collect::<Vec<&str>>().join("\n    "))
}