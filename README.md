
work happens in [src/instrument/add_hooks/mod.rs](https://github.com/cdetrio/wasabi/blob/taint-io-flow/src/instrument/add_hooks/mod.rs), [src/instrument/add_hooks/taint_io.rs](https://github.com/cdetrio/wasabi/blob/taint-io-flow/src/instrument/add_hooks/taint_io.rs), [src/instrument/add_hooks/taint_stack.rs](https://github.com/cdetrio/wasabi/blob/taint-io-flow/src/instrument/add_hooks/taint_stack.rs).


```
$ cargo build
$ ./target/debug/wasabi keccak256_c.wasm

.......
.......
```
--------------------------------------------
function IO flow maps. for each function, which input params and globals taint the return var.

The data structure is currently named "Taint Input-Output Flow Map", because it represents a map of the data flow from a function's inputs to its output return val.

The initial motivation for the IO Flow Map is to make it easy to propagate taints in main() through function calls and back.
```
function 3 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 1, in_flow_set: [Constant] }
function 4 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 23, in_flow_set: [Constant, LocalVar(Local 2)] }
function 4 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 27, in_flow_set: [LocalVar(Local 0)] }
function 5 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 20, in_flow_set: [Constant, LocalVar(Local 2)] }
function 5 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 28, in_flow_set: [LocalVar(Local 2), LocalVar(Local 0)] }
function 6 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 625, in_flow_set: [Constant] }
function 7 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 65, in_flow_set: [Constant] }
function 8 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 1, in_flow_set: [Constant] }
function 9 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 26, in_flow_set: [Constant] }
function 9 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 94, in_flow_set: [Constant] }
function 9 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 121, in_flow_set: [Constant, MemoryVal] }
function 10 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 24, in_flow_set: [Constant, MemoryVal] }
function 10 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 30, in_flow_set: [Constant] }
function 11 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 171, in_flow_set: [MemoryVal] }
function 11 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 193, in_flow_set: [MemoryVal, Constant] }
function 11 has IO flow var: VarTaintRecord { flow_var: ReturnVal, var_position: Instr 204, in_flow_set: [Constant, MemoryVal] }
function 12 has IO flow var: VarTaintRecord { flow_var: BrIfConditionalVal, var_position: Instr 15, in_flow_set: [MemoryVal] }
```
--------------------------------------------
function "loop controls". for each br_if that branches to a loop (branch target is a loop instruction), the conditional var determines how many iterations of the loop will be executed.

If all the loop-targeting br_ifs have conditional vars that are taint class `constant` (i.e. are derived from propagated constants), then the runtime is constant for all inputs. loops can be unrolled during metering and a simple gas formula is easy to generate (ideal case for upper-bound analysis)

If a loop-targeting br_if has a conditional var that is taint class `inputSize`, then runtime is dependendent on input length, i.e. only input length and not on input value (second-best case for upper-bound analysis)

If a loop-targeting br_if has a conditional var that is taint calss `inputVal`, then runtime is dependent on input value (worst-case complexity, most difficult to find upper bound)

note that functions other than main() are still inaccurate, because we aren't yet propagating taints across functions. Right now `InputVal` means it is tainted by a function input param. But all functions are ultimately called from main(), so their input params' taints should flow from the vars passed from main. We need to propagate taints from main() to all other functions and back, in order to see the correct taint of loop-targeting br_ifs in all other functions. TODO: fix this.
```
function 4 loop control: BrToLoop { br_conditional_var: InputVal, br_position: Instr 23, br_target: BranchTarget { absolute_instr: Instr 6, ended_blocks: [Loop { begin: Instr 6, end: Instr 24 }] } }
function 5 loop control: BrToLoop { br_conditional_var: InputVal, br_position: Instr 20, br_target: BranchTarget { absolute_instr: Instr 8, ended_blocks: [Loop { begin: Instr 8, end: Instr 21 }] } }
function 6 loop control: BrToLoop { br_conditional_var: Constant, br_position: Instr 625, br_target: BranchTarget { absolute_instr: Instr 206, ended_blocks: [Loop { begin: Instr 206, end: Instr 626 }] } }
function 7 loop control: BrToLoop { br_conditional_var: InputSize, br_position: Instr 65, br_target: BranchTarget { absolute_instr: Instr 33, ended_blocks: [Loop { begin: Instr 33, end: Instr 66 }] } }
function 9 loop control: BrToLoop { br_conditional_var: Constant, br_position: Instr 26, br_target: BranchTarget { absolute_instr: Instr 13, ended_blocks: [Loop { begin: Instr 13, end: Instr 27 }] } }
function 11 loop control: BrToLoop { br_conditional_var: MemoryVal, br_position: Instr 171, br_target: BranchTarget { absolute_instr: Instr 164, ended_blocks: [Loop { begin: Instr 164, end: Instr 172 }] } }
function 11 loop control: BrToLoop { br_conditional_var: MemoryVal, br_position: Instr 193, br_target: BranchTarget { absolute_instr: Instr 179, ended_blocks: [Loop { begin: Instr 179, end: Instr 194 }] } }
function 12 loop control: BrToLoop { br_conditional_var: MemoryVal, br_position: Instr 15, br_target: BranchTarget { absolute_instr: Instr 8, ended_blocks: [Loop { begin: Instr 8, end: Instr 16 }] } }
```