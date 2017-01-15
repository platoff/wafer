
import value

type
  Code* = enum
    CODE_NULL, CODE_FALSE, CODE_TRUE, CODE_POP, CODE_CLOSE_UPVALUE, CODE_RETURN, CODE_END, 
    CODE_LOAD_LOCAL_0, CODE_LOAD_LOCAL_1, CODE_LOAD_LOCAL_2, CODE_LOAD_LOCAL_3, CODE_LOAD_LOCAL_4, 
    CODE_LOAD_LOCAL_5, CODE_LOAD_LOCAL_6, CODE_LOAD_LOCAL_7, CODE_LOAD_LOCAL_8, 
    CODE_CONSTRUCT, CODE_FOREIGN_CONSTRUCT, CODE_FOREIGN_CLASS
    
    CODE_CONSTANT, CODE_LOAD_MODULE_VAR, CODE_STORE_MODULE_VAR, CODE_CALL_0, CODE_CALL_1, CODE_CALL_2,
    CODE_CALL_3, CODE_CALL_4, CODE_CALL_5, CODE_CALL_6, CODE_CALL_7, CODE_CALL_8, CODE_CALL_9, CODE_CALL_10,
    CODE_CALL_11, CODE_CALL_12, CODE_CALL_13, CODE_CALL_14, CODE_CALL_15, CODE_CALL_16,
    CODE_JUMP, CODE_LOOP, CODE_JUMP_IF, CODE_AND, CODE_OR, CODE_METHOD_INSTANCE, CODE_METHOD_STATIC,

    CODE_LOAD_LOCAL, CODE_STORE_LOCAL, CODE_LOAD_UPVALUE, CODE_STORE_UPVALUE, 
    CODE_LOAD_FIELD_THIS, CODE_STORE_FIELD_THIS, CODE_LOAD_FIELD, CODE_STORE_FIELD, 
    CODE_CLASS,

    CODE_SUPER_0, CODE_SUPER_1, CODE_SUPER_2, CODE_SUPER_3,
    CODE_SUPER_4, CODE_SUPER_5, CODE_SUPER_6, CODE_SUPER_7,
    CODE_SUPER_8, CODE_SUPER_9, CODE_SUPER_10, CODE_SUPER_11,
    CODE_SUPER_12, CODE_SUPER_13, CODE_SUPER_14, CODE_SUPER_15,
    CODE_SUPER_16

    CODE_CLOSURE


# Returns the number of arguments to the instruction at [ip] in [fn]'s
# bytecode.
proc getNumArguments(bytecode: HugeArray[byte], constants: HugeArray[Value], ip: int): int =
  case bytecode[ip].Code:
  of CODE_NULL, CODE_FALSE, CODE_TRUE, CODE_POP, CODE_CLOSE_UPVALUE, CODE_RETURN, CODE_END, 
      CODE_LOAD_LOCAL_0, CODE_LOAD_LOCAL_1, CODE_LOAD_LOCAL_2, CODE_LOAD_LOCAL_3, CODE_LOAD_LOCAL_4, 
      CODE_LOAD_LOCAL_5, CODE_LOAD_LOCAL_6, CODE_LOAD_LOCAL_7, CODE_LOAD_LOCAL_8, 
      CODE_CONSTRUCT, CODE_FOREIGN_CONSTRUCT, CODE_FOREIGN_CLASS: 0

  of CODE_LOAD_LOCAL, CODE_STORE_LOCAL, CODE_LOAD_UPVALUE, CODE_STORE_UPVALUE, 
      CODE_LOAD_FIELD_THIS, CODE_STORE_FIELD_THIS, CODE_LOAD_FIELD, CODE_STORE_FIELD, CODE_CLASS: 1

  of CODE_CONSTANT, CODE_LOAD_MODULE_VAR, CODE_STORE_MODULE_VAR, CODE_CALL_0, CODE_CALL_1, CODE_CALL_2,
      CODE_CALL_3, CODE_CALL_4, CODE_CALL_5, CODE_CALL_6, CODE_CALL_7, CODE_CALL_8, CODE_CALL_9, CODE_CALL_10,
      CODE_CALL_11, CODE_CALL_12, CODE_CALL_13, CODE_CALL_14, CODE_CALL_15, CODE_CALL_16,
      CODE_JUMP, CODE_LOOP, CODE_JUMP_IF, CODE_AND, CODE_OR, CODE_METHOD_INSTANCE, CODE_METHOD_STATIC: 2

  of CODE_SUPER_0, CODE_SUPER_1, CODE_SUPER_2, CODE_SUPER_3, CODE_SUPER_4, CODE_SUPER_5, CODE_SUPER_6,
      CODE_SUPER_7, CODE_SUPER_8, CODE_SUPER_9, CODE_SUPER_10, CODE_SUPER_11, CODE_SUPER_12, CODE_SUPER_13,
      CODE_SUPER_14, CODE_SUPER_15, CODE_SUPER_16: 4

  of CODE_CLOSURE:
    let constant = (bytecode[ip + 1].int shl 8) or bytecode[ip + 2].int
    let loadedFn = asFn(constants[constant])

    # There are two bytes for the constant, then two for each upvalue.
    2 + (loadedFn.numUpvalues * 2)
  else:
    assert false, "unreachable"
    0

proc bindMethodCode*(classObj: ObjClass, fn: ObjFn) =
  var ip = 0
  while true:
    let instruction = (Code)fn.code[ip]
    inc ip
    case instruction:
    of CODE_LOAD_FIELD, CODE_STORE_FIELD, 
      CODE_LOAD_FIELD_THIS, CODE_STORE_FIELD_THIS:
        # Shift this class's fields down past the inherited ones. We don't
        # check for overflow here because we'll see if the number of fields
        # overflows when the subclass is created.
        fn.code[ip] = byte(fn.code[ip].int + classObj.superclass.numFields)
        inc ip

    of CODE_SUPER_0, CODE_SUPER_1, CODE_SUPER_2, CODE_SUPER_3,
       CODE_SUPER_4, CODE_SUPER_5, CODE_SUPER_6, CODE_SUPER_7,
       CODE_SUPER_8, CODE_SUPER_9, CODE_SUPER_10, CODE_SUPER_11,
       CODE_SUPER_12, CODE_SUPER_13, CODE_SUPER_14, CODE_SUPER_15,
       CODE_SUPER_16:
        # Skip over the symbol.
        inc ip, 2
        
        # Fill in the constant slot with a reference to the superclass.
        let constant = (fn.code[ip].int shl 8) or fn.code[ip + 1].int
        fn.constants[constant] = asVal(classObj.superclass)

    of CODE_CLOSURE:
        # Bind the nested closure too.
        let constant = (fn.code[ip].int shl 8) or fn.code[ip + 1].int
        bindMethodCode(classObj, asFn(fn.constants[constant]))
        inc ip, getNumArguments(fn.code.data, fn.constants.data, ip - 1)

    of CODE_END:
        break

    else:
        # Other instructions are unaffected, so just skip over them.
        inc ip, getNumArguments(fn.code.data, fn.constants.data, ip - 1)
