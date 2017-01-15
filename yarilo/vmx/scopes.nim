
include parser

const 
  stackEffects: array[Code, int] = [
    1
  ]

# Emits one single-byte argument. Returns its index.
proc emitByte(compiler: var Compiler, arg: int): int =
  compiler.gc.add(compiler.fn.code, arg.byte)
  
  # Assume the instruction is associated with the most recently consumed token.
  # compiler.gc.add(compiler->parser->vm, &compiler->fn->debug->sourceLines,
  #                    compiler->parser->previous.line);
  
  result = int(compiler.fn.code.count) - 1

# Emits one bytecode instruction.
proc emitOp(compiler: var Compiler, instruction: Code) =
  discard emitByte(compiler, instruction.int)
  
  # Keep track of the stack's high water mark.
  inc compiler.numSlots, stackEffects[instruction]
  if compiler.numSlots > compiler.fn.maxSlots:
    compiler.fn.maxSlots = compiler.numSlots

# Emits one 16-bit argument, which will be written big endian.
proc emitShort(compiler: var Compiler, arg: int) =
  discard emitByte(compiler, (arg shr 8) and 0xff)
  discard emitByte(compiler, arg and 0xff)

# Emits one bytecode instruction followed by a 8-bit argument. Returns the
# index of the argument in the bytecode.
proc emitByteArg(compiler: var Compiler, instruction: Code, arg: int): int =
  emitOp(compiler, instruction)
  result = emitByte(compiler, arg)

# Emits one bytecode instruction followed by a 16-bit argument, which will be
# written big endian.
proc emitShortArg(compiler: var Compiler, instruction: Code, arg: int) =
  emitOp(compiler, instruction)
  emitShort(compiler, arg)

