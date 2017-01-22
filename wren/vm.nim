
import model
import opcodes
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

# Handles both foreign methods where [methodValue] is a string containing the
# method's signature and Wren methods where [methodValue] is a function.

# Aborts the current fiber if the method is a foreign method that could not be
# found.
proc bindMethod(vm: VM, methodType: int, symbol: int,
                module: ObjModule, clazz: ObjClass, methodValue: Value) =
  let classObj = 
    if methodType == CODE_METHOD_STATIC.int:
      clazz.obj.classObj
    else:
      clazz
  let className = classObj.name.c_value

  var meth: Method
  if methodValue.isString:
    let name = vmcast[ObjString](methodValue).c_value
    meth.kind = METHOD_FOREIGN
    meth.foreign = vm.findForeignMethod(module.name.c_value,
                                          className,
                                          methodType == CODE_METHOD_STATIC.int,
                                          name)

    if meth.foreign == nil:
      vm.fiber.error = stringFormat(vm, 
        "Could not find foreign method '@' for class $ in module '$'.").val
      # ,
      #      methodValue, classObj.name.c_value, module.name.c_value)
  else:
    meth.kind = METHOD_BLOCK
    meth.obj = vmcast[ObjClosure](methodValue)

    # Patch up the bytecode now that we know the superclass.
    bindMethodCode(classObj, meth.obj.fn)

  bindMethod(vm, classObj, symbol, meth)

type 
  InterpretResult* = enum
    Success
    RuntimeError
    CompileError

# Aborts the current fiber with an appropriate method not found error for a
# method with [symbol] on [classObj].
proc methodNotFound(vm: VM, classObj: ObjClass, symbol: int) =
  vm.fiber.error = stringFormat(vm, "$ does not implement '$'.",
    classObj.name.c_value, vm.methodNames.data[symbol].buffer).val

# Checks that [value], which must be a closure, does not require more
# parameters than are provided by [numArgs].
#
# If there are not enough arguments, aborts the current fiber and returns
# `false`.
proc checkArity(vm: VM, value: Value, numArgs: int): bool =
  assert(isClosure(value), "Receiver must be a closure.")
  let fn = vmcast[ObjClosure](value).fn

  # We only care about missing arguments, not extras. The "- 1" is because
  # numArgs includes the receiver, the function itself, which we don't want to
  # count.
  if numArgs - 1 >= fn.arity:
    result = true
  else:
    vm.fiber.error = newString(vm, "Function expects more arguments.").val
    result = false

proc ensureStack(vm: VM, fiber: ObjFiber, needed: int) =
  if fiber.stackCapacity >= needed:
    return

  echo "not implemented"

# Adds a new [CallFrame] to [fiber] invoking [closure] whose stack starts at
# [stackStart].
proc appendCallFrame(vm: VM, fiber: ObjFiber,
                                closure: ObjClosure, stackStart: PArray[Value]) =
  # The caller should have ensured we already have enough capacity.
  assert(fiber.frameCapacity > fiber.numFrames, "No memory for call frame.")
  
  let frame = addr fiber.frames[fiber.numFrames]
  inc fiber.numFrames
  frame.stackStart = stackStart
  frame.closure = closure
  frame.ip = closure.fn.code.data

# Pushes [closure] onto [fiber]'s callstack and invokes it. Expects [numArgs]
# arguments (including the receiver) to be on the top of the stack already.
proc callFunction(
    vm: VM, fiber: ObjFiber, closure: ObjClosure, numArgs: int) =
  # Grow the call frame array if needed.
  echo "call Function"
  if fiber.numFrames + 1 > fiber.frameCapacity:
    let max = fiber.frameCapacity * 2
    fiber.frames = cast[PArray[CallFrame]](reallocate(vm, fiber.frames,
        sizeof(CallFrame) * fiber.frameCapacity,
        sizeof(CallFrame) * max))
    fiber.frameCapacity = max

  # Grow the stack if needed.
  let stackSize = (cast[int](fiber.stackTop) -% cast[int](fiber.stack)) div sizeof(Value)
  let needed = stackSize + closure.fn.maxSlots
  ensureStack(vm, fiber, needed)
  
  appendCallFrame(vm, fiber, closure, cast[PArray[Value]](addr fiber.stackTop[-numArgs]))
  

proc debugPrintStackTrace(vm: VM) =
  echo "print stack trace here"

# Closes any open upvates that have been created for stack slots at [last] and
# above.
proc closeUpvalues(fiber: ObjFiber, last: ptr Value) =
  let lastAddress = cast[uint](last)
  while fiber.openUpvalues != nil and
         cast[uint](fiber.openUpvalues.value) >= lastAddress:
    let upvalue = fiber.openUpvalues

    # Move the value into the upvalue itself and point the upvalue to it.
    upvalue.closed = upvalue.value[]
    upvalue.value = addr upvalue.closed

    # Remove it from the open upvalue list.
    fiber.openUpvalues = upvalue.next

# Captures the local variable [local] into an [Upvalue]. If that local is
# already in an upvalue, the existing one will be used. (This is important to
# ensure that multiple closures closing over the same variable actually see
# the same variable.) Otherwise, it will create a new open upvalue and add it
# the fiber's list of upvalues.
proc captureUpvalue(vm: VM, fiber: ObjFiber, local: ptr Value): ObjUpvalue =
  # If there are no open upvalues at all, we must need a new one.
  if fiber.openUpvalues == nil:
    fiber.openUpvalues = newUpvalue(vm, local)
    return fiber.openUpvalues

  var prevUpvalue: ObjUpvalue = nil
  var upvalue = fiber.openUpvalues

  # Walk towards the bottom of the stack until we find a previously existing
  # upvalue or pass where it should be.
  while upvalue != nil and upvalue.value > local:
    prevUpvalue = upvalue
    upvalue = upvalue.next

  # Found an existing upvalue for this local.
  if upvalue != nil and upvalue.value == local: 
    return upvalue

  # We've walked past this local on the stack, so there must not be an
  # upvalue for it already. Make a new one and link it in in the right
  # place to keep the list sorted.
  result = newUpvalue(vm, local)
  if prevUpvalue == nil:
    # The new one is the first one in the list.
    fiber.openUpvalues = result
  else:
    prevUpvalue.next = result

  result.next = upvalue

# Verifies that [superclassValue] is a valid object to inherit from. That
# means it must be a class and cannot be the class of any built-in type.
#
# Also validates that it doesn't result in a class with too many fields and
# the other limitations foreign classes have.
#
# If successful, returns `null`. Otherwise, returns a string for the runtime
# error message.
proc validateSuperclass(vm: VM, name, superclassValue: Value,
                                numFields: int): Value =
  # Make sure the superclass is a class.
  if not isCLASS(superclassValue):
    return stringFormat(vm,
      "Class '@' cannot inherit from a non-class object.", cstring("xxx")).val

  # Make sure it doesn't inherit from a sealed built-in type. Primitive methods
  # on these classes assume the instance is one of the other Obj___ types and
  # will fail horribly if it's actually an ObjInstance.
  let superclass = vmcast[ObjClass](superclassValue)
  if superclass == vm.classClass or
      superclass == vm.fiberClass or
      superclass == vm.fnClass or
      superclass == vm.listClass or
      superclass == vm.mapClass or
      superclass == vm.rangeClass or
      superclass == vm.stringClass:
    return stringFormat(vm,
        "Class '@' cannot inherit from built-in class '@'.",
        "name".cstring, "OBJ_VAL(superclass->name)".cstring).val

  if superclass.numFields == -1:
    return stringFormat(vm,
        "Class '@' cannot inherit from foreign class '@'.",
        "name".cstring, "OBJ_VAL(superclass->name)".cstring).val

  if numFields == -1 and superclass.numFields > 0:
    return stringFormat(vm,
        "Foreign class '@' may not inherit from a class with fields.",
        "name".cstring).val

  if superclass.numFields + numFields > MAX_FIELDS:
    return stringFormat(vm,
        "Class '@' may not have more than 255 fields, including inherited ones.", "name".cstring).val

  return NullVal
  
# Creates a new class.
#
# If [numFields] is -1, the class is a foreign class. The name and superclass
# should be on top of the fiber's stack. After calling this, the top of the
# stack will contain the new class.
#
# Aborts the current fiber if an error occurs.
proc createClass(vm: VM, numFields: int, module: ObjModule) =
  # Pull the name and superclass off the stack.
  let name = vm.fiber.stackTop[-2]
  let superclass = vm.fiber.stackTop[-1]

  # We have two values on the stack and we are going to leave one, so discard
  # the other slot.
  dec vm.fiber.stackTop

  vm.fiber.error = validateSuperclass(vm, name, superclass, numFields)
  if not isNull(vm.fiber.error):
    return

  let classObj = newClass(vm, vmcast[ObjClass](superclass), numFields,
                                    vmcast[ObjString](name))
  vm.fiber.stackTop[-1] = classObj.val

  if numFields == -1:
    echo "foreign classes not supported"


# Handles the current fiber having aborted because of an error. Switches to
# a new fiber if there is a fiber that will handle the error, otherwise, tells
# the VM to stop.
proc runtimeError(vm: VM) =
  assert(not isNull(vm.fiber.error), "Should only call this after an error.")

  # Unhook the caller since we will never resume and return to it.
  let caller = vm.fiber.caller
  vm.fiber.caller = nil

  # If the caller ran this fiber using "try", give it the error.
  if vm.fiber.callerIsTrying:
    # Make the caller's try method return the error message.
    caller.stackTop[-1] = vm.fiber.error

    vm.fiber = caller
    return

  # If we got here, nothing caught the error, so show the stack trace.
  debugPrintStackTrace(vm)
  vm.fiber = nil
  vm.apiStack = nil

proc runInterpreter(vm: VM, fib: ObjFiber): InterpretResult =
  var fiber = fib
  vm.fiber = fiber

  # Hoist these into local variables. They are accessed frequently in the loop
  # but assigned less frequently. Keeping them in locals and updating them when
  # a call frame has been pushed or popped gives a large speed boost.
  var frame: ptr CallFrame
  var stackStart: PArray[Value]
  var ip: PArray[byte]
  var fn: ObjFn
  
  # Use this after a CallFrame has been pushed or popped to refresh the local
  # variables.
  proc loadFrame() =
    frame = addr fiber.frames[fiber.numFrames - 1]
    stackStart = frame.stackStart
    ip = frame.ip
    fn = frame.closure.fn

  proc peek(): Value = fiber.stackTop[-1]
  proc peek2(): Value = fiber.stackTop[-2]

  proc pop(): Value = 
    dec fiber.stackTop
    result = fiber.stackTop[0]

  proc push(val: Value) = 
    fiber.stackTop[0] = val
    inc fiber.stackTop

  proc drop() = 
    dec fiber.stackTop

  # Use this before a CallFrame is pushed to store the local variables back
  # into the current one.
  proc storeFrame() =
    frame.ip = ip

  proc readByte(): int =
    echo "IP: ", cast[int](ip) -% cast[int](frame.ip)
    inc ip
    result = ip[-1].int

  proc readShort(): int =
    inc ip, 2
    result = (ip[-2].int shl 8) or ip[-1].int

            

  loadFrame()

  while true:
    let instruction = readByte()
    case (Code)instruction:
    of CODE_LOAD_LOCAL_0,
      CODE_LOAD_LOCAL_1,
      CODE_LOAD_LOCAL_2,
      CODE_LOAD_LOCAL_3,
      CODE_LOAD_LOCAL_4,
      CODE_LOAD_LOCAL_5,
      CODE_LOAD_LOCAL_6,
      CODE_LOAD_LOCAL_7,
      CODE_LOAD_LOCAL_8:
        push(stackStart[instruction.int - CODE_LOAD_LOCAL_0.int])

    of CODE_LOAD_LOCAL:
        push(stackStart[readByte().int])

    of CODE_LOAD_FIELD_THIS:
        let field = readByte().int
        let receiver = stackStart[0]
        assert(isInstance(receiver), "Receiver should be instance.")
        let instance = vmcast[ObjInstance](receiver)
        assert(field < instance.obj.classObj.numFields, "Out of bounds field.")
        push(instance.fields[field])

    of CODE_POP:   drop()
    of CODE_NULL:  push(NullVal)
    of CODE_FALSE: push(FalseVal)
    of CODE_TRUE:  push(TrueVal)

    of CODE_STORE_LOCAL:
        stackStart[readByte().int] = peek()

    of CODE_CONSTANT:
        echo "!!Constant!!"
        push(fn.constants[readShort()])

    of CODE_CALL_0,
      CODE_CALL_1,
      CODE_CALL_2,
      CODE_CALL_3,
      CODE_CALL_4,
      CODE_CALL_5,
      CODE_CALL_6,
      CODE_CALL_7,
      CODE_CALL_8,
      CODE_CALL_9,
      CODE_CALL_10,
      CODE_CALL_11,
      CODE_CALL_12,
      CODE_CALL_13,
      CODE_CALL_14,
      CODE_CALL_15,
      CODE_CALL_16:
        echo "!!! CALL"
        # Add one for the implicit receiver argument.
        let numArgs = instruction - CODE_CALL_0.int + 1
        let symbol = readShort()

        # The receiver is the first argument.
        let args = cast[PArray[Value]](cast[int](fiber.stackTop) -% (sizeof(Value) * numArgs))
        let classObj = getClass(vm, args[0])
        assert classObj != nil, "object has no class " & $cast[Obj](args[0].obj).kind
        
        ## Complete Call
        # If the class's method table doesn't include the symbol, fail.
        let meth = classObj.methods[symbol]
        if symbol >= classObj.methods.len or
          meth.kind == METHOD_NONE:
          echo "Method not found"
          methodNotFound(vm, classObj, symbol)
  #  template runtimeError(): InterpretResult =
          storeFrame()
          runtimeError(vm)
          if vm.fiber == nil:
            return RuntimeError
          fiber = vm.fiber
          loadFrame()

        case meth.kind:
        of METHOD_PRIMITIVE:
            if meth.primitive(vm, args):
              # The result is now in the first arg slot. Discard the other
              # stack slots.
              dec fiber.stackTop, numArgs - 1
            else:
              # An error or fiber switch occurred.
              storeFrame()

              # If we don't have a fiber to switch to, stop interpreting.
              fiber = vm.fiber
              if fiber == nil: 
                return Success
              if not isNull(fiber.error):
                return RuntimeError
              
              loadFrame()

        of METHOD_FOREIGN:
              return RuntimeError

        of METHOD_FN_CALL:
            echo "method fn call"
            if not checkArity(vm, args[0], numArgs):
              return RuntimeError

            storeFrame()
            vm.callFunction(fiber, vmcast[ObjClosure](args[0]), numArgs)
            loadFrame()

        of METHOD_BLOCK:
            echo ">> method block CALL"
            storeFrame()
            callFunction(vm, fiber, meth.obj, numArgs)
            loadFrame()

        of METHOD_NONE:
            assert false, "unreachable"

    of CODE_SUPER_0,
      CODE_SUPER_1,
      CODE_SUPER_2,
      CODE_SUPER_3,
      CODE_SUPER_4,
      CODE_SUPER_5,
      CODE_SUPER_6,
      CODE_SUPER_7,
      CODE_SUPER_8,
      CODE_SUPER_9,
      CODE_SUPER_10,
      CODE_SUPER_11,
      CODE_SUPER_12,
      CODE_SUPER_13,
      CODE_SUPER_14,
      CODE_SUPER_15,
      CODE_SUPER_16:
        # Add one for the implicit receiver argument.
        let numArgs = instruction - CODE_SUPER_0.int + 1
        let symbol = readShort()

        # The receiver is the first argument.
        let args = cast[PArray[Value]](cast[int](fiber.stackTop) -% (sizeof(Value) * numArgs))

        # The superclass is stored in a constant.
        let classObj = vmcast[ObjClass](fn.constants[readShort()])
      
        ## Complete Call
        # If the class's method table doesn't include the symbol, fail.
        let meth = classObj.methods[symbol]
        if symbol >= classObj.methods.len or
          meth.kind == METHOD_NONE:
          methodNotFound(vm, classObj, symbol)
  #  template runtimeError(): InterpretResult =
          storeFrame()
          runtimeError(vm)
          if vm.fiber == nil:
            return RuntimeError
          fiber = vm.fiber
          loadFrame()
        
        case meth.kind:
        of METHOD_PRIMITIVE:
            if meth.primitive(vm, args):
              # The result is now in the first arg slot. Discard the other
              # stack slots.
              dec fiber.stackTop, numArgs - 1
            else:
              # An error or fiber switch occurred.
              storeFrame()

              # If we don't have a fiber to switch to, stop interpreting.
              fiber = vm.fiber
              if fiber == nil: 
                return Success
              if not isNull(fiber.error):
                return RuntimeError
              
              loadFrame()

        of METHOD_FOREIGN:
              echo " not implemented"
              return RuntimeError

        of METHOD_FN_CALL:
            if not checkArity(vm, args[0], numArgs):
              return RuntimeError

            storeFrame()
            vm.callFunction(fiber, vmcast[ObjClosure](args[0]), numArgs)
            loadFrame()

        of METHOD_BLOCK:
            storeFrame()
            callFunction(vm, fiber, meth.obj, numArgs)
            loadFrame()

        of METHOD_NONE:
            assert false, "unreachable"

    of CODE_LOAD_UPVALUE:
        push(frame.closure.upvalues[readByte()].value[])
    of CODE_STORE_UPVALUE:
        frame.closure.upvalues[readByte()].value[] = peek()

    of CODE_LOAD_MODULE_VAR:
        push(fn.module.variables[readShort()])

    of CODE_STORE_MODULE_VAR:
        fn.module.variables[readShort()] = peek()

    of CODE_STORE_FIELD_THIS:
        let field = readByte()
        let receiver = stackStart[0]
        assert(isInstance(receiver), "Receiver should be instance.")
        let instance = vmcast[ObjInstance](receiver)
        assert(field < instance.obj.classObj.numFields, "Out of bounds field.")
        instance.fields[field] = peek()

    of CODE_LOAD_FIELD:
        let field = readByte()
        let receiver = pop()
        assert(isInstance(receiver), "Receiver should be instance.")
        let instance = vmcast[ObjInstance](receiver)
        assert(field < instance.obj.classObj.numFields, "Out of bounds field.")
        instance.fields[field].push

    of CODE_STORE_FIELD:
        let field = readByte()
        let receiver = pop()
        assert(isInstance(receiver), "Receiver should be instance.")
        let instance = vmcast[ObjInstance](receiver)
        assert(field < instance.obj.classObj.numFields, "Out of bounds field.")
        instance.fields[field] = peek()

    of CODE_JUMP:
        let offset = readShort()
        inc ip, offset

    of CODE_LOOP:
        # Jump back to the top of the loop.
        let offset = readShort()
        dec ip, offset

    of CODE_JUMP_IF:
        let offset = readShort()
        let condition = pop()

        if isFalse(condition) or isNull(condition): inc ip, offset

    of CODE_AND:
        let offset = readShort()
        let condition = peek()

        if isFalse(condition) or isNull(condition):
          # Short-circuit the right hand side.
          inc ip, offset
        else:
          # Discard the condition and evaluate the right hand side.
          drop()

    of CODE_OR:
        let offset = readShort()
        let condition = peek()

        if isFalse(condition) or isNull(condition):
          # Discard the condition and evaluate the right hand side.
          drop()
        else:
          # Short-circuit the right hand side.
          inc ip, offset

    of CODE_CLOSE_UPVALUE:
        # Close the upvalue for the local if we have one.
        closeUpvalues(fiber, addr fiber.stackTop[-1])
        drop()

    of CODE_RETURN:
        echo "!! RET"
        let res = pop()
        dec fiber.numFrames

        # Close any upvalues still in scope.
        closeUpvalues(fiber, addr stackStart[0])

        # If the fiber is complete, end it.
        if fiber.numFrames == 0:
          # See if there's another fiber to return to. If not, we're done.
          if fiber.caller == nil:
            # Store the final result value at the beginning of the stack so the
            # C API can get it.
            fiber.stack[0] = res
            fiber.stackTop = cast[PArray[Value]](addr fiber.stack[1])
            return Success
          
          let resumingFiber = fiber.caller
          fiber.caller = nil
          fiber = resumingFiber
          vm.fiber = resumingFiber
          
          # Store the result in the resuming fiber.
          fiber.stackTop[-1] = res
        else:
          # Store the result of the block in the first slot, which is where the
          # caller expects it.
          stackStart[0] = res

          # Discard the stack slots for the call frame (leaving one slot for the
          # result).
          fiber.stackTop = cast[PArray[Value]](addr fiber.stack[1])
        
        loadFrame()

    of CODE_CONSTRUCT:
        assert(isClass(stackStart[0]), "'this' should be a class.")
        stackStart[0] = newInstance(vm, vmcast[ObjClass](stackStart[0])).val

    of CODE_FOREIGN_CONSTRUCT:
        assert false, "not implemented foreign"

    of CODE_CLOSURE:
        echo "!!Closure!!"
        # Create the closure and push it on the stack before creating upvalues
        # so that it doesn't get collected.
        let function = vmcast[ObjFn](fn.constants[readShort()])
        let closure = newClosure(vm, function)
        push(closure.val)

        # Capture upvalues, if any.
        for i in 0..<function.numUpvalues:
          let isLocal = readByte()
          let index = readByte()
          if isLocal != 0:
            # Make an new upvalue to close over the parent's local variable.
            closure.upvalues[i] = captureUpvalue(vm, fiber,
                                                  addr frame.stackStart[index])
          else:
            # Use the same upvalue as the current call frame.
            closure.upvalues[i] = frame.closure.upvalues[index]

    of CODE_CLASS:
        createClass(vm, readByte(), nil)
        if not isNull(fiber.error): 
          echo "problem create class"
          return RuntimeError

    of CODE_FOREIGN_CLASS:
      echo "not impletementd"
      return RuntimeError

    of CODE_METHOD_INSTANCE,
      CODE_METHOD_STATIC:
        let symbol = readShort()
        let classObj = vmcast[ObjClass](peek())
        let meth = peek2()
        bindMethod(vm, instruction, symbol, fn.module, classObj, meth)
        if not isNull(fiber.error):
          echo "RTE"
          return RuntimeError
        drop()
        drop()

    of CODE_END:
        # A CODE_END should always be preceded by a CODE_RETURN. If we get here,
        # the compiler generated wrong code.
        assert false, "unrechable"

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

