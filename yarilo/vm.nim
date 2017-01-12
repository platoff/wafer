
import utils
import value
import module
import map

type
  VM* = ptr WrenVM
  WrenVM = object
    gcObj: GCObj
    modules: ObjMap  
    methodNames: SymbolTable

proc gc*(vm: VM): GC = addr vm.gcObj

proc initializeCore(vm: VM) =
  let gc = vm.gc
  var coreModule = gc.newModule(NullVal.asString)
  gc.mapSet(vm.modules, NullVal, coreModule)

proc newVM*(initialHeapSize: int): VM =
  result = create WrenVM
  init(result.gcObj, initialHeapSize)
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
  var module = vm.getModule(name)
  if module == nil:
    var newModule = vm.gc.newModule(name.asString)
    vm.gc.mapSet(vm.modules, name, newModule)
    #   module = m

    # # Implicitly import the core module.
    # let coreModule = getModule(vm, NullVal)
    # for i in 0..<int(coreModule.variables.len):
    #   discard defineVariable(vm, module,
    #                       coreModule.variableNames[i].buffer,
    #                       coreModule.variableNames[i].length,
    #                       coreModule.variables[i])


when isMainModule:
  var vm = newVM(65536)