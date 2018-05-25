/*
 * Wasabi loader (monkey-patches WebAssembly.instantiate()) and runtime (e.g., for resolving call_indirect).
 */

let Wasabi = {
    HOOK_NAMES: [
        "if_",
        "br",
        "br_if",
        "br_table",
        "begin",
        "end",
        "nop",
        "unreachable",
        "drop",
        "select",
        "call_pre",
        "call_post",
        "return_",
        "const_",
        "unary",
        "binary",
        "load",
        "store",
        "memory_size",
        "memory_grow",
        "local",
        "global"
    ],

    // map a table index to a function index
    resolveTableIdx: function (tableIdx) {
        if (Wasabi.module.exports === undefined || Wasabi.module.table === undefined) {
            console.warn("cannot resolve table index without exports and table (possible reason: exports and table are not available during Wasm start function)");
            return undefined;
        }

        // find which export matches the function object in the table
        let func = Wasabi.module.table.get(tableIdx);
        let [funcExportName, _x] = Object.entries(Wasabi.module.exports).find(([funcName, funcObject]) => funcObject === func);

        // map exported function name back to function index
        let funcIdx = Wasabi.module.info.functions.findIndex(func => func.export === funcExportName);
        if (funcIdx < 0) throw `could not find ${funcExportName} in module exports, but every function should be exported by Wasabi?`;
        return funcIdx;
    },

    module: {
        // filled at instrumentation time
        info: {}, lowlevelHooks: {},
        // filled after instantiation
        exports: undefined, table: undefined,
    },
};

// provide analysis callback stubs if they were not defined by the user
for (const hook of Wasabi.HOOK_NAMES) {
    if (window[hook] === undefined) {
        window[hook] = function () {
        };
    }
}

// monkey-patch WebAssembly functions to add Wasabi
{
    const oldInstantiate = WebAssembly.instantiate;
    WebAssembly.instantiate = (sourceBuffer, importObject) => {
        if (Wasabi.module.lowlevelHooks === undefined) {
            throw "missing low-level hooks, did you include the JavaScript file generated by Wasabi during instrumentation?";
        }

        let importObjectWithHooks = importObject || {};
        importObjectWithHooks.__wasabi_hooks = Wasabi.module.lowlevelHooks;

        const result = oldInstantiate(sourceBuffer, importObjectWithHooks);
        // as soon as instance is available, save exports and table
        result.then(({module, instance}) => {
            Wasabi.module.exports = instance.exports;
            Wasabi.module.table = instance.exports[Wasabi.module.info.tableExportName];
        });
        return result;
    };

    // just fall-back to regular instantiation since Wasabi doesn't support streaming instrumentation (yet) anyway
    const oldInstantiateStreaming = WebAssembly.instantiateStreaming;
    WebAssembly.instantiateStreaming = async (source, importObject) => {
        let response = await source;
        let buffer = await response.arrayBuffer();
        return WebAssembly.instantiate(buffer, importObject);
    };

    WebAssembly.Instance = () => {
        throw "synchronous WebAssembly instantiation is not supported by Wasabi (Chrome does not support it for >4KB modules anyway, so we cannot execute Wasabi itself synchronously)"
    };
}