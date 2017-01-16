
include value
include opcodes

# Looks up the previously loaded module with [name].
#
# Returns `NULL` if no module with that name has been loaded.
proc getModule(vm: WrenVM, name: Value): ObjModule =
  let moduleValue = vm.modules[name]
  if not moduleValue.isUndefined: 
    result = vmcast[ObjModule](moduleValue)

# The maximum number of module-level variables that may be defined at one time.
# This limitation comes from the 16 bits used for the arguments to
# `CODE_LOAD_MODULE_VAR` and `CODE_STORE_MODULE_VAR`.
const 
  MaxModuleVars = 65536

proc defineVariable*(vm: var WrenVM, module: ObjModule, name: cstring,
                     length: int, value: Value): int =
  if module.variables.len == MaxModuleVars: 
    return -2

  if value.isObj:
    vm.pushRoot(value.asObj)  # TODO: do we need this here?

  # See if the variable is already explicitly or implicitly declared.
  result = module.variableNames.find(name, length)
  if result == -1:
    # Brand new variable.
    result = vm.add(module.variableNames, name, length)
    vm.add(module.variables, value)
  elif isNum(module.variables[result]):
    ## An implicitly declared variable's value will always be a number. Now we
    ## have a real definition.
    module.variables[result] = value
  else:
    # Already explicitly declared.
    result = -1

  if value.isObj:
    vm.popRoot(value.asObj)

proc loadModule*(vm: var WrenVM, name: Value, source: cstring) =
  var module = vm.getModule(name)
  if module == nil:
    let newModule = vm.newModule(vmcast[ObjString](name))
    vm.pushRoot newModule.asObj
    vm.mapSet(vm.modules, name, newModule)
    vm.popRoot newModule.asObj

    # Implicitly import the core module.
    let coreModule = getModule(vm, NullVal)
    for i in 0..<coreModule.variables.len:
      discard vm.defineVariable(module,
                          coreModule.variableNames[i].buffer,
                          coreModule.variableNames[i].length,
                          coreModule.variables[i])

# Looks up a foreign method in [moduleName] on [className] with [signature].
#
# This will try the host's foreign method binder first. If that fails, it
# falls back to handling the built-in modules.
proc findForeignMethod(vm: WrenVM; moduleName, className: cstring;
                              isStatic: bool; signature: cstring): ForeignMethodFn =
  echo "not implemented"

# Defines [methodValue] as a method on [classObj].
#
# Handles both foreign methods where [methodValue] is a string containing the
# method's signature and Wren methods where [methodValue] is a function.
#
# Aborts the current fiber if the method is a foreign method that could not be
# found.
proc bindMethod(vm: var WrenVM, methodType: int, symbol: int,
                module: ObjModule, clazz: ObjClass, methodValue: Value) =
  let classObj = 
    if methodType == CODE_METHOD_STATIC.int:
      clazz.obj.classObj
    else:
      clazz
  let className = classObj.name.value

  var meth: Method
  if methodValue.isString:
    let name = vmcast[ObjString](methodValue).value
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
    meth.obj = vmcast[ObjClosure](methodValue)

    # Patch up the bytecode now that we know the superclass.
    bindMethodCode(classObj, meth.obj.fn)

  bindMethod(vm, classObj, symbol, meth)

