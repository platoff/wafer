
import model
import opcodes

proc `$`(str: ObjString): string = 
  if str == nil or str.value == nil: "<nil>" else: $str.c_value

proc `$`(obj: Obj): string =
  "of class [" & $obj.kind & "]"

proc `$`(val: Value): string =
  if val.isObj: $val.asObj
  elif val.isNum: $val.asNum
  else: "<value>"

proc dumpInstruction(vm: VM, fn: ObjFn, start: int, lastLine: var int): int =
  var i = start
  let bytecode = fn.code.data
  let code = (Code)bytecode[i]

  let line = fn.debug.sourceLines.data[i]
  if lastLine != line:
    write(stdout, "#" & $line & ":\n")
    lastLine = line

  write(stdout, $i)
  write(stdout, "  ")
  inc i

  proc READ_BYTE(): int = 
    result = bytecode[i].int
    inc i

  proc READ_SHORT(): int =
    inc i, 2
    result = (bytecode[i - 2].int shl 8) or bytecode[i - 1].int

  proc BYTE_INSTRUCTION(name: string) =
    echo name, " ", READ_BYTE()

  case code:
  of CODE_CONSTANT:
      let constant = READ_SHORT()
      echo "CONSTANT: ", constant, " ", fn.constants.data[constant]
  of CODE_NULL:  echo "NULL"
  of CODE_FALSE: echo "FALSE"
  of CODE_TRUE:  echo "TRUE"

  of CODE_LOAD_LOCAL_0: echo "LOAD_LOCAL_0"
  of CODE_LOAD_LOCAL_1: echo "LOAD_LOCAL_1"
  of CODE_LOAD_LOCAL_2: echo "LOAD_LOCAL_2"
  of CODE_LOAD_LOCAL_3: echo "LOAD_LOCAL_3"
  of CODE_LOAD_LOCAL_4: echo "LOAD_LOCAL_4"
  of CODE_LOAD_LOCAL_5: echo "LOAD_LOCAL_5"
  of CODE_LOAD_LOCAL_6: echo "LOAD_LOCAL_6"
  of CODE_LOAD_LOCAL_7: echo "LOAD_LOCAL_7"
  of CODE_LOAD_LOCAL_8: echo "LOAD_LOCAL_8"

  of CODE_LOAD_LOCAL: BYTE_INSTRUCTION("LOAD_LOCAL")
  of CODE_STORE_LOCAL: BYTE_INSTRUCTION("STORE_LOCAL")
  of CODE_LOAD_UPVALUE: BYTE_INSTRUCTION("LOAD_UPVALUE")
  of CODE_STORE_UPVALUE: BYTE_INSTRUCTION("STORE_UPVALUE")
  
  of CODE_LOAD_MODULE_VAR:
    let slot = READ_SHORT()
    echo "LOAD_MODULE_VAR ", slot, " ",
           fn.module.variableNames.data[slot].buffer

  of CODE_STORE_MODULE_VAR:
    let slot = READ_SHORT()
    echo "STORE_MODULE_VAR ", slot, " ",
           fn.module.variableNames.data[slot].buffer

  of CODE_LOAD_FIELD_THIS: BYTE_INSTRUCTION("LOAD_FIELD_THIS")
  of CODE_STORE_FIELD_THIS: BYTE_INSTRUCTION("STORE_FIELD_THIS")
  of CODE_LOAD_FIELD: BYTE_INSTRUCTION("LOAD_FIELD")
  of CODE_STORE_FIELD: BYTE_INSTRUCTION("STORE_FIELD")

  of CODE_POP: echo "POP"

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
      let numArgs = bytecode[i - 1].int - CODE_CALL_0.int
      let symbol = READ_SHORT()
      echo "CALL_", numArgs, " ", symbol, " ", vm.methodNames.data[symbol].buffer
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
      let numArgs = bytecode[i - 1].int - CODE_SUPER_0.int
      let symbol = READ_SHORT()
      let superclass = READ_SHORT()
      echo "SUPER_", numArgs," ", symbol, " ",
            vm.methodNames.data[symbol].buffer, superclass
  of CODE_JUMP:
    let offset = READ_SHORT()
    echo "JUMP ", offset, " to ", i + offset
  of CODE_LOOP:
    let offset = READ_SHORT()
    echo "LOOP ", offset, " to ", i - offset
  of CODE_JUMP_IF:
    let offset = READ_SHORT()
    echo "JUMP_IF ", offset, " to ", i + offset
  of CODE_AND:
    let offset = READ_SHORT()
    echo "AND ", offset, " to ", i + offset
  of CODE_OR:
    let offset = READ_SHORT()
    echo "OR ", offset, " to ", i + offset
  of CODE_CLOSE_UPVALUE: echo "CLOSE_UPVALUE"
  of CODE_RETURN: echo "RETURN"
  of CODE_CLOSURE:
    let constant = READ_SHORT()
    echo "CLOSURE ", constant
  #   wrenDumpValue(fn->constants.data[constant]);
  #   printf(" ");
  #   ObjFn* loadedFn = AS_FN(fn->constants.data[constant]);
  #   for (int j = 0; j < loadedFn->numUpvalues; j++)
  #   {
  #     int isLocal = READ_BYTE();
  #     int index = READ_BYTE();
  #     if (j > 0) printf(", ");
  #     printf("%s %d", isLocal ? "local" : "upvalue", index);
  #   }
  #   printf("\n");
  #   break;
  # }

  of CODE_CONSTRUCT:  echo "CONSTRUCT"
  of CODE_FOREIGN_CONSTRUCT: echo "FOREIGN_CONSTRUCT"      
  of CODE_CLASS:
    let numFields = READ_BYTE()
    echo "CLASS ", numFields
  of CODE_FOREIGN_CLASS: echo "FOREIGN_CLASS"
  of CODE_METHOD_INSTANCE:
    let symbol = READ_SHORT()
    echo "METHOD_INSTANCE ", symbol, " ",
            vm.methodNames.data[symbol].buffer
  of CODE_METHOD_STATIC:
    let symbol = READ_SHORT()
    echo "METHOD_STATIC ", symbol, " ",
            vm.methodNames.data[symbol].buffer
  of CODE_END: echo "END"
  else:
    echo "UKNOWN! ", bytecode[i - 1].int

  # Return how many bytes this instruction takes, or -1 if it's an END.
  result = if code == CODE_END: -1 else: i - start


proc dumpCode*(vm: VM, fn: ObjFn) =
  echo fn.module.name, " ", fn.debug.name
  var i = 0
  var lastLine = -1
  while true:
    let offset = dumpInstruction(vm, fn, i, lastLine)
    if offset == -1: 
      break;
    inc i, offset

  echo ""

  