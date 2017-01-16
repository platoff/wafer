
import model
import compiler

# Looks up the previously loaded module with [name].
#
# Returns `NULL` if no module with that name has been loaded.
proc getModule*(vm: VM, name: Value): ObjModule =
  let moduleValue = vm.modules[name]
  if not moduleValue.isUndefined: 
    result = vmcast[ObjModule](moduleValue)

proc loadModule*(vm: VM, name: Value, source: cstring): ObjFiber =
  var module = vm.getModule(name)
  if module == nil:
    module = vm.newModule(vmcast[ObjString](name))
    vm.pushRoot module
    vm.mapSet(vm.modules, name, module.val)
    vm.popRoot module

    # Implicitly import the core module.
    let coreModule = getModule(vm, NullVal)
    assert coreModule != nil
    for i in 0..<coreModule.variables.len:
      discard vm.defineVariable(module,
                          coreModule.variableNames[i].buffer,
                          coreModule.variableNames[i].length,
                          coreModule.variables[i])

  let fn = vm.compile(module, source, false, true)
  if fn == nil:
    return nil
  
  vm.pushRoot fn
  let closure = vm.newClosure(fn)
  vm.pushRoot closure
  result = vm.newFiber(closure)
  vm.popRoot closure
  vm.popRoot fn

# Looks up a foreign method in [moduleName] on [className] with [signature].
#
# This will try the host's foreign method binder first. If that fails, it
# falls back to handling the built-in modules.
proc findForeignMethod(vm: VM; moduleName, className: cstring;
                              isStatic: bool; signature: cstring): ForeignMethodFn =
  echo "not implemented foreign method"

# Defines [methodValue] as a method on [classObj].
#
# Handles both foreign methods where [methodValue] is a string containing the
# method's signature and Wren methods where [methodValue] is a function.
#
# Aborts the current fiber if the method is a foreign method that could not be
# found.
# proc bindMethod(vm: VM, methodType: int, symbol: int,
#                 module: ObjModule, clazz: ObjClass, methodValue: Value) =
#   let classObj = 
#     if methodType == CODE_METHOD_STATIC.int:
#       clazz.obj.classObj
#     else:
#       clazz
#   let className = classObj.name.c_value

#   var meth: Method
#   if methodValue.isString:
#     let name = vmcast[ObjString](methodValue).c_value
#     meth.kind = METHOD_FOREIGN
#     meth.foreign = vm.findForeignMethod(module.name.c_value,
#                                           className,
#                                           methodType == CODE_METHOD_STATIC.int,
#                                           name)

#     if meth.foreign == nil:
#       vm.fiber.error = stringFormat(vm, 
#         "Could not find foreign method '@' for class $ in module '$'.").val
#       # ,
#       #      methodValue, classObj.name.c_value, module.name.c_value)
#   else:
#     meth.kind = METHOD_BLOCK
#     meth.obj = vmcast[ObjClosure](methodValue)

#     # Patch up the bytecode now that we know the superclass.
#     bindMethodCode(classObj, meth.obj.fn)

#   bindMethod(vm, classObj, symbol, meth)

type 
  InterpretResult* = enum
    CompileError

proc runInterpreter(vm: VM, fiber: ObjFiber): InterpretResult =
  echo "TODO: magic!"

proc interpretInModule*(vm: VM, module, source: cstring): InterpretResult =
  var nameValue: ObjString = nil
  if module != nil:
    nameValue = vm.newString(module)
    vm.pushRoot(nameValue)

  let fiber = vm.loadModule(if nameValue == nil: NullVal else: nameValue.val, source)  ## nil -> NullVal
  if module != nil: vm.popRoot(nameValue)

  result = if fiber == nil: CompileError else: runInterpreter(vm, fiber)

proc interpret*(vm: VM, source: cstring): InterpretResult =
  interpretInModule(vm, "main", source)

