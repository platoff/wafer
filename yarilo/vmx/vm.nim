
include gc

type
  VM[GC,C] = ptr WrenVM[GC,C]
  WrenVM[GC,C] = object
    gc: GC
    compiler: C
    modules: ObjMap  
    methodNames: SymbolTable

proc gc(vm: VM): var GC = vm.gcInstance

proc initializeCore(vm: VM) =
  let gc = vm.gc
  let system = gc.newString("system")
  gc.pushRoot system
  let coreModule = gc.newModule(system)
  gc.pushRoot coreModule
  gc.mapSet(vm.modules, system.asVal, coreModule.asVal)
  gc.popRoot coreModule
  gc.popRoot system

proc newVM[GC,C](gc: GC, compiler: C): VM[GC,C] =
  result = create WrenVM[GC,C]
  result.gc = gc
  result.compiler = compiler
  result.gc.newMap(result.modules) # TODO: mapClass is nil, as in original... hope it's OK
  result.initializeCore

# Looks up the previously loaded module with [name].
#
# Returns `NULL` if no module with that name has been loaded.
proc getModule(vm: VM, name: Value): ObjModule =
  let moduleValue = vm.modules[name]
  if not moduleValue.isUndefined: 
    result = moduleValue.asModule

proc loadModule(vm: VM, name: Value, source: cstring) =
  let gc = vm.gc
  var module = vm.getModule(name)
  if module == nil:
    let newModule = gc.newModule(name.asString)
    gc.pushRoot newModule    
    gc.mapSet(vm.modules, name, newModule.asVal)
    gc.popRoot newModule

    let system = gc.newString("system")
    gc.pushRoot system
    # Implicitly import the core module.
    let coreModule = getModule(vm, system.asVal)
    for i in 0..<int(coreModule.variables.count):
      discard gc.defineVariable(module,
                          coreModule.variableNames[i].buffer,
                          coreModule.variableNames[i].length,
                          coreModule.variables[i])

    gc.popRoot system
