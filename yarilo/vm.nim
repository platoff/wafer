
# import value
# import gc
# import opcodes

include gc
include opcodes

type
  VM* = ptr WrenVM
  WrenVM = object
    gc*: GC
    fiber*: ObjFiber
    modules*: ObjMap  
    methodNames*: SymbolTable

proc newVM*(gc: GC): VM =
  result = create WrenVM
  result.gc = gc
#  result.compiler = compiler
  result.gc.newMap(result.modules) # TODO: mapClass is nil, as in original... hope it's OK

# Looks up the previously loaded module with [name].
#
# Returns `NULL` if no module with that name has been loaded.
proc getModule(vm: VM, name: Value): ObjModule =
  let moduleValue = vm.modules[name]
  if not moduleValue.isUndefined: 
    result = moduleValue.asModule

# The maximum number of module-level variables that may be defined at one time.
# This limitation comes from the 16 bits used for the arguments to
# `CODE_LOAD_MODULE_VAR` and `CODE_STORE_MODULE_VAR`.
const 
  MaxModuleVars = 65536

proc defineVariable*[GC](gc: GC, module: ObjModule, name: cstring,
                       length: int, value: Value): int =
  if module.variables.len == MaxModuleVars: 
    return -2

  if value.isObj:
    gc.pushRoot(value.asObj)

  # See if the variable is already explicitly or implicitly declared.
  result = module.variableNames.find(name, length)
  if result == -1:
    # Brand new variable.
    result = gc.add(module.variableNames, name, length)
    gc.add(module.variables, value)
  elif isNum(module.variables[result]):
    ## An implicitly declared variable's value will always be a number. Now we
    ## have a real definition.
    module.variables[result] = value
  else:
    # Already explicitly declared.
    result = -1

  if value.isObj:
    gc.popRoot(value.asObj)

proc loadModule*(vm: VM, name: Value, source: cstring) =
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
    for i in 0..<coreModule.variables.len:
      discard gc.defineVariable(module,
                          coreModule.variableNames[i].buffer,
                          coreModule.variableNames[i].length,
                          coreModule.variables[i])

    gc.popRoot system


# Looks up a foreign method in [moduleName] on [className] with [signature].
#
# This will try the host's foreign method binder first. If that fails, it
# falls back to handling the built-in modules.
proc findForeignMethod(vm: VM; moduleName, className: cstring;
                              isStatic: bool; signature: cstring): ForeignMethodFn =
  echo "not implemented"

# Defines [methodValue] as a method on [classObj].
#
# Handles both foreign methods where [methodValue] is a string containing the
# method's signature and Wren methods where [methodValue] is a function.
#
# Aborts the current fiber if the method is a foreign method that could not be
# found.
proc bindMethod(vm: VM, methodType: int, symbol: int,
                module: ObjModule, clazz: ObjClass, methodValue: Value) =
  let classObj = 
    if methodType == CODE_METHOD_STATIC.int:
      clazz.getClazz
    else:
      clazz
  let className = classObj.name.value

  var meth: Method
  if methodValue.isString:
    let name = methodValue.asString.value
    meth.kind = METHOD_FOREIGN
    meth.foreign = vm.findForeignMethod(module.name.value,
                                          className,
                                          methodType == CODE_METHOD_STATIC.int,
                                          name)

    if meth.foreign == nil:
      # vm.fiber.error = wrenStringFormat(vm,
      #     "Could not find foreign method '@' for class $ in module '$'.",
      #     methodValue, classObj->name->value, module->name->value);
      echo "format"
  else:
    meth.kind = METHOD_BLOCK
    meth.obj = methodValue.asClosure

    # Patch up the bytecode now that we know the superclass.
    bindMethodCode(classObj, meth.obj.fn)

  bindMethod(vm.gc, classObj, symbol, meth)

