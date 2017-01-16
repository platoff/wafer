
import model
import opcodes
import debug 

const
  MaxInterpolationNesting = 8
# The maximum number of distinct constants that a function can contain. This
# value is explicit in the bytecode since `CODE_CONSTANT` only takes a single
# two-byte argument.
  MAX_CONSTANTS = 1 shl 16
# The maximum distance a CODE_JUMP or CODE_JUMP_IF instruction can move the
# instruction pointer.
  MAX_JUMP = 1 shl 16

  LF = char(10)

type
  TokenKind = enum
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACKET,
    TOKEN_RIGHT_BRACKET,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COLON,
    TOKEN_DOT,
    TOKEN_DOTDOT,
    TOKEN_DOTDOTDOT,
    TOKEN_COMMA,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_LTLT,
    TOKEN_GTGT,
    TOKEN_PIPE,
    TOKEN_PIPEPIPE,
    TOKEN_CARET,
    TOKEN_AMP,
    TOKEN_AMPAMP,
    TOKEN_BANG,
    TOKEN_TILDE,
    TOKEN_QUESTION,
    TOKEN_EQ,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_EQEQ,
    TOKEN_BANGEQ,

    TOKEN_BREAK,
    TOKEN_CLASS,
    TOKEN_CONSTRUCT,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FOREIGN,
    TOKEN_IF,
    TOKEN_IMPORT,
    TOKEN_IN,
    TOKEN_IS,
    TOKEN_NULL,
    TOKEN_RETURN,
    TOKEN_STATIC,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_FIELD,
    TOKEN_STATIC_FIELD,
    TOKEN_NAME,
    TOKEN_NUMBER,
  
  # A string literal without any interpolation, or the last section of a
  # string following the last interpolated expression.
    TOKEN_STRING,
  
  # // A portion of a string literal preceding an interpolated expression. This
  # // string:
  # //
  # //     "a %(b) c %(d) e"
  # //
  # // is tokenized to:
  # //
  # //     TOKEN_INTERPOLATION "a "
  # //     TOKEN_NAME          b
  # //     TOKEN_INTERPOLATION " c "
  # //     TOKEN_NAME          d
  # //     TOKEN_STRING        " e"
    TOKEN_INTERPOLATION,

    TOKEN_LINE,

    TOKEN_ERROR,
    TOKEN_EOF

  Token = object
    kind: TokenKind
    start: cstring
    length: int
    line: int
    value: Value

  Parser = object
    vm: VM
    # The module being parsed.
    module: ObjModule

    source: cstring
    tokenStart: cstring
    currentChar: cstring
    currentLine: int
    current: Token
    previous: Token

    parens: array[MaxInterpolationNesting, int]
    numParens: int

    skipNewlines: bool
    printErrors: bool
    hasError: bool

const
# The maximum number of local (i.e. not module level) variables that can be
# declared in a single function, method, or chunk of top level code. This is
# the maximum number of variables in scope at one time, and spans block scopes.

# Note that this limitation is also explicit in the bytecode. Since
# `CODE_LOAD_LOCAL` and `CODE_STORE_LOCAL` use a single argument byte to
# identify the local, only 256 can be in scope at one time.
  MaxLocals = 256
  MaxUpvalues = 256

type
  Local = object
    name: cstring
    length: int
    depth: int
    isUpvalue: bool

  CompilerUpvalue = object
    isLocal: bool
    index: int

  Loop = object
    start: int
    exitJump: int
    body: int
    scopeDepth: int
    enclosing: ptr Loop

  SignatureKind = enum
    skMethod
    skGetter
    skSetter
    skSubscript
    skSubscriptSetter
    skInitializer

  Signature = object
    name: cstring
    length: int
    kind: SignatureKind    
    arity: int

  ClassCompiler = object
    name: ObjString
    fields: SymbolTable
    methods: Buffer[int]
    staticMethods: Buffer[int]

    isForeign: bool
    inStatic: bool

    signature: ptr Signature

  Compiler = ptr WrenCompiler
  WrenCompiler = object
    parser: ptr Parser
    parent: Compiler
    locals: array[MaxLocals, Local]
    numLocals: int
    upvalues: array[MaxUpvalues, CompilerUpvalue]
    scopeDepth: int
    numSlots: int
  # The current innermost loop being compiled, or NULL if not in a loop. 
    loop: ptr Loop
    enclosingClass: ptr ClassCompiler
  # The function being compiled.  
    fn: ObjFn
    constants: ObjMap

  Scope = enum
    SCOPE_LOCAL,
    SCOPE_UPVALUE,
    SCOPE_MODULE

# A reference to a variable and the scope where it is defined. This contains
# enough information to emit correct code to load or store the variable.
  Variable = object
  # The stack slot, upvalue slot, or module symbol defining the variable.
    index: int  
  # Where the variable is declared.
    scope: Scope

proc error(compiler: Compiler, format: cstring) {.noinline.} =
  echo "error: ", format
  raise newException(Exception, "x")

# Adds [constant] to the constant pool and returns its index.
proc addConstant(compiler: Compiler, constant: Value): int =
  if compiler.parser.hasError:
    return -1
  
  # See if we already have a constant for the value. If so, reuse it.
  if compiler.constants != nil:
    let existing = compiler.constants[constant]
    if isNum(existing):
      return (int)asNum(existing)
  
  # It's a new constant.
  if compiler.fn.constants.len < MAX_CONSTANTS:
    if constant.isObj:
      pushRoot(compiler.parser.vm, constant.asObj)
    add(compiler.parser.vm, compiler.fn.constants, constant)
    if constant.isObj:
      popRoot(compiler.parser.vm, constant.asObj)
    
    if compiler.constants == nil:
      compiler.constants = newMap(compiler.parser.vm)
    mapSet(compiler.parser.vm, compiler.constants, constant,
               val(compiler.fn.constants.len - 1))
  else:
    error(compiler, "A function may only contain %d unique constants.")

  result = compiler.fn.constants.len - 1

proc init(compiler: var WrenCompiler, parser: ptr Parser, parent: Compiler, isFunction: bool) =
  compiler.parser = parser
  compiler.parent = parent
  compiler.loop = nil
  compiler.enclosingClass = nil

  compiler.fn = nil
  compiler.constants = nil

  if parent == nil:
    compiler.numLocals = 0
    compiler.scopeDepth = -1
  else:
    # Declare a fake local variable for the receiver so that it's slot in the
    # stack is taken. For methods, we call this "this", so that we can resolve
    # references to that like a normal variable. For functions, they have no
    # explicit "this". So we pick a bogus name. That way references to "this"
    # inside a function will try to walk up the parent chain to find a method
    # enclosing the function whose "this" we can close over.
    compiler.numLocals = 1
    if isFunction:
      compiler.locals[0].name = nil
      compiler.locals[0].length = 0
    else:
      compiler.locals[0].name = "this"
      compiler.locals[0].length = 4
    compiler.locals[0].depth = -1
    compiler.locals[0].isUpvalue = false

    # The initial scope for function or method is a local scope.
    compiler.scopeDepth = 0
  
  compiler.numSlots = compiler.numLocals
  compiler.fn = parser.vm.newFunction(parser.module, compiler.numLocals)

##
## Lexing -----------------
##

type
  Keyword = object
    ident: cstring
    length: int
    kind: TokenKind

template keyword(i: cstring, l: int, k: TokenKind): Keyword =
  Keyword(ident: i, length: l, kind: k)

const
  keywords = [
    keyword("break",     5, TOKEN_BREAK),
    keyword("class",     5, TOKEN_CLASS),
    keyword("construct", 9, TOKEN_CONSTRUCT),
    keyword("else",      4, TOKEN_ELSE),
    keyword("false",     5, TOKEN_FALSE),
    keyword("for",       3, TOKEN_FOR),
    keyword("foreign",   7, TOKEN_FOREIGN),
    keyword("if",        2, TOKEN_IF),
    keyword("import",    6, TOKEN_IMPORT),
    keyword("in",        2, TOKEN_IN),
    keyword("is",        2, TOKEN_IS),
    keyword("null",      4, TOKEN_NULL),
    keyword("return",    6, TOKEN_RETURN),
    keyword("static",    6, TOKEN_STATIC),
    keyword("super",     5, TOKEN_SUPER),
    keyword("this",      4, TOKEN_THIS),
    keyword("true",      4, TOKEN_TRUE),
    keyword("var",       3, TOKEN_VAR),
    keyword("while",     5, TOKEN_WHILE),
    keyword(nil,         0, TOKEN_EOF) # Sentinel to mark the end of the arra),    
  ]

var errno {.importc, header: "<errno.h>".}: cint

proc c_strtod(buf: cstring, endptr: ptr cstring): float64 {.
  importc: "strtod", header: "<stdlib.h>", noSideEffect.}

proc isName(c: char): bool =
  (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_'

proc isDigit(c: char): bool =
  (c >= '0' and c <= '9')

proc lexError(parser: Parser, error: cstring) {.noinline.} =
  echo error

proc inc(p: var cstring) = 
  p = cast[cstring](cast[int](p) +% 1)

proc dec(p: var cstring) = 
  p = cast[cstring](cast[int](p) -% 1)

proc peekChar(parser: Parser): char = parser.currentChar[0]

# Returns the character after the current character.
proc peekNextChar(parser: Parser): char =
  # If we're at the end of the source, don't read past it.
  if peekChar(parser) == char(0):
    result = '\0'
  else:
    result = parser.currentChar[1]

proc nextChar(parser: var Parser): char = 
  result = parser.peekChar
  inc parser.currentChar
  if result == LF:
    inc parser.currentLine

proc matchChar(parser: var Parser, c: char): bool =
  if parser.peekChar == c:
    inc parser.currentChar
    result = true

proc makeToken(parser: var Parser, kind: TokenKind) =
  parser.current.kind = kind
  parser.current.start = parser.tokenStart
  parser.current.length = 
    cast[int](parser.currentChar) - cast[int](parser.tokenStart)
  parser.current.line = parser.currentLine
  if kind == TOKEN_LINE:
    dec parser.current.line

proc makeNumber(parser: var Parser, isHex: bool) =
  errno = 0
  if isHex:
    parser.lexError "hex not supported"
  else:
    parser.current.value = val(c_strtod(parser.tokenStart, nil))

  if errno != 0:
    parser.lexError "parsing error"
    parser.current.value = val(0)

  parser.makeToken TOKEN_NUMBER

proc readHexNumber(parser: var Parser) =
  echo "not implemented readHex"

proc readNumber(parser: var Parser) =
  while parser.peekChar.isDigit:
    discard parser.nextChar
  
  # See if it has a floating point. Make sure there is a digit after the "."
  # so we don't get confused by method calls on number literals.
  if parser.peekChar == '.' and parser.peekNextChar.isDigit:
    discard parser.nextChar
    while parser.peekChar.isDigit: discard parser.nextChar

  if parser.peekChar == 'e' or parser.peekChar == 'E':
    # Allow a negative exponent.
    discard matchChar(parser, '-')
    
    if not isDigit(peekChar(parser)):
      lexError(parser, "Unterminated scientific notation.")
    
    while isDigit(peekChar(parser)): discard nextChar(parser)

  parser.makeNumber false

proc readName(parser: var Parser, kind: TokenKind) =
  var tokenKind = kind
  while parser.peekChar.isName or parser.peekChar.isDigit:
    discard parser.nextChar
  let length = cast[int](parser.currentChar) - cast[int](parser.tokenStart)
  for i in 0..<keywords.len:
    if length == keywords[i].length and
        equalMem(parser.tokenStart, keywords[i].ident, length):
      tokenKind = keywords[i].kind
      break
  parser.makeToken tokenKind

proc readString(parser: var Parser) =
  var 
    str: Buffer[char]
    kind = TOKEN_STRING

  while true:
    let c = parser.nextChar
    case c:
    of '"':
      break
    of char(0):
      parser.lexError("Unterminated string.")
      dec parser.currentChar
      break
    of '%':
      if parser.numParens < MaxInterpolationNesting:
        if parser.nextChar != '(':
          parser.lexError "Expect '(' after '%%'."
        parser.parens[parser.numParens] = 1
        inc parser.numParens
        kind = TOKEN_INTERPOLATION
        break
      parser.lexError "Interpolation may only nest %d levels deep."
    of '\\':
      case nextChar(parser):
      of '"':  add(parser.vm, str, '"')
      of '\\': add(parser.vm, str, '\\')
      of '%':  add(parser.vm, str, '%')
      of '0':  add(parser.vm, str, '\0')
      of 'a':  add(parser.vm, str, '\a')
      of 'b':  add(parser.vm, str, '\b')
      of 'f':  add(parser.vm, str, '\f')
      of 'n':  add(parser.vm, str, LF)
      of 'r':  add(parser.vm, str, '\r')
      of 't':  add(parser.vm, str, '\t')
      of 'v':  add(parser.vm, str, '\v')
      of 'u', #:  add(parser.str, 4)
         'U', #:  add(parser.str, 8)
         'x':
        #add(parser->vm, str, (uint8_t)readHexEscape(parser, 2, "byte"));
        echo "not implemented 0xUu"
      else:
        lexError(parser, "Invalid escape character '%c'.")
#                   *(parser->currentChar - 1));
    else:
      parser.vm.add(str, c)
      
  parser.current.value = parser.vm.newString(cast[cstring](str.data), str.len).val
  parser.vm.clear(str)
  parser.makeToken kind

proc twoCharToken(parser: var Parser, c: char, two, one: TokenKind) =
  makeToken(parser, if matchChar(parser, c): two else: one)

proc skipLineComment(parser: var Parser) =
  while peekChar(parser) != LF and peekChar(parser) != char(0):
    discard nextChar(parser)

proc skipBlockComment(parser: var Parser) =
  echo "not implemented block comment"

proc nextToken(parser: var Parser) =
  parser.previous = parser.current
  if parser.current.kind == TOKEN_EOF:
    return

  while parser.peekChar != '\0':
    parser.tokenStart = parser.currentChar
    let c = parser.nextChar
    case c
    of '(':
      if parser.numParens > 0:
        inc parser.parens[parser.numParens - 1]
      parser.makeToken TOKEN_LEFT_PAREN
      return
    of ')':
      if parser.numParens > 0:
        dec parser.parens[parser.numParens - 1]
        if parser.parens[parser.numParens - 1] == 0:
          dec parser.numParens
          parser.readString
          return
      parser.makeToken TOKEN_RIGHT_PAREN
      return
    of '[': 
      makeToken(parser, TOKEN_LEFT_BRACKET)
      return
    of ']': 
      makeToken(parser, TOKEN_RIGHT_BRACKET)
      return      
    of '{': 
      makeToken(parser, TOKEN_LEFT_BRACE)
      return      
    of '}': 
      makeToken(parser, TOKEN_RIGHT_BRACE)
      return      
    of ':': 
      makeToken(parser, TOKEN_COLON)
      return      
    of ',': 
      makeToken(parser, TOKEN_COMMA)
      return      
    of '*': 
      makeToken(parser, TOKEN_STAR)
      return      
    of '%': 
      makeToken(parser, TOKEN_PERCENT)
      return      
    of '^': 
      makeToken(parser, TOKEN_CARET)
      return      
    of '+': 
      makeToken(parser, TOKEN_PLUS)
      return      
    of '-': 
      makeToken(parser, TOKEN_MINUS)
      return      
    of '~': 
      makeToken(parser, TOKEN_TILDE)
      return      
    of '?': 
      makeToken(parser, TOKEN_QUESTION)
      return      
    of '|': 
      twoCharToken(parser, '|', TOKEN_PIPEPIPE, TOKEN_PIPE)
      return
    of '&': 
      twoCharToken(parser, '&', TOKEN_AMPAMP, TOKEN_AMP)
      return
    of '=': 
      twoCharToken(parser, '=', TOKEN_EQEQ, TOKEN_EQ)
      return
    of '!': 
      twoCharToken(parser, '=', TOKEN_BANGEQ, TOKEN_BANG)
      return
    of '.':
      if parser.matchChar '.':
        parser.twoCharToken '.', TOKEN_DOTDOTDOT, TOKEN_DOTDOT
        return
      parser.makeToken TOKEN_DOT
      return
    of '/':
      if parser.matchChar '/':
        parser.skipLineComment
      elif parser.matchChar '*':
        parser.skipBlockComment
      else:
        parser.makeToken TOKEN_SLASH
        return
    of '<':
      if parser.matchChar '<':
        parser.makeToken TOKEN_LTLT
      else:
        parser.twoCharToken '=', TOKEN_LTEQ, TOKEN_LT
      return
    of '>':
      if parser.matchChar '>':
        parser.makeToken TOKEN_GTGT
      else:
        parser.twoCharToken '=', TOKEN_GTEQ, TOKEN_GT
      return
    of LF:
      parser.makeToken TOKEN_LINE
      return
    of ' ', '\r', '\t':
      while parser.peekChar == ' ' or parser.peekChar == '\r' or
            parser.peekChar == '\t':
        discard parser.nextChar
    of '"':
      parser.readString
      return
    of '_':
      parser.readName if parser.peekChar == '_': TOKEN_STATIC_FIELD else: TOKEN_FIELD
      return
    of '0':
      if parser.peekChar == 'x':
        parser.readHexNumber
      else:
        parser.readNumber
      return
    else:
      if parser.currentLine == 1 and c == '#' and parser.peekChar == '!':
        parser.skipLineComment
      else:
        if c.isName:
          parser.readName TOKEN_NAME
        elif c.isDigit:
          parser.readNumber
        else:
          parser.lexError "Invalid character '%c'."
        return

  parser.tokenStart = parser.currentChar
  parser.makeToken TOKEN_EOF

##
## Parsing ---------------------------
##


# Returns the type of the current token.
proc peek(compiler: Compiler): TokenKind =
  compiler.parser.current.kind

# Consumes the current token if its type is [expected]. Returns true if a
# token was consumed.
proc match(compiler: Compiler, expected: TokenKind): bool =
  if peek(compiler) != expected:
    result = false
  else:
    nextToken(compiler.parser[])
    result = true

# Consumes the current token. Emits an error if its type is not [expected].
proc consume(compiler: Compiler, expected: TokenKind, 
    errorMessage: cstring) =
  nextToken(compiler.parser[])
#  echo "expect: ", expected,  " actual: ", compiler.parser.previous.kind  
  if compiler.parser.previous.kind != expected:
    error(compiler, errorMessage)

    # If the next token is the one we want, assume the current one is just a
    # spurious error and discard it to minimize the number of cascaded errors.
    if compiler.parser.current.kind == expected:
      nextToken(compiler.parser[])

# Matches one or more newlines. Returns true if at least one was found.
proc matchLine(compiler: Compiler): bool =
  if not match(compiler, TOKEN_LINE):
    return false

  while match(compiler, TOKEN_LINE):
    discard
  result = true


# Discards any newlines starting at the current token.
proc ignoreNewlines(compiler: Compiler) =
  discard matchLine(compiler)

# Consumes the current token. Emits an error if it is not a newline. Then
# discards any duplicate newlines following it.
proc consumeLine(compiler: Compiler, errorMessage: cstring) =
  consume(compiler, TOKEN_LINE, errorMessage)
  ignoreNewlines(compiler)

##
## Variables and Scopes --------------
##

# Emits one single-byte argument. Returns its index.
proc emitByte(compiler: Compiler, arg: int): int =
  compiler.parser.vm.add(compiler.fn.code, arg.byte)
  
  #Assume the instruction is associated with the most recently consumed token.
  compiler.parser.vm.add(compiler.fn.debug.sourceLines,
                          compiler.parser.previous.line)
  
  result = compiler.fn.code.len - 1

# Emits one bytecode instruction.
proc emitOp(compiler: Compiler, instruction: Code) =
  discard emitByte(compiler, instruction.int)
  
  # Keep track of the stack's high water mark.
  inc compiler.numSlots, stackEffects[instruction]
  if compiler.numSlots > compiler.fn.maxSlots:
    compiler.fn.maxSlots = compiler.numSlots

# Emits one 16-bit argument, which will be written big endian.
proc emitShort(compiler: Compiler, arg: int) =
  discard emitByte(compiler, (arg shr 8) and 0xff)
  discard emitByte(compiler, arg and 0xff)

# Emits one bytecode instruction followed by a 8-bit argument. Returns the
# index of the argument in the bytecode.
proc emitByteArg(compiler: Compiler, instruction: Code, arg: int): int =
  emitOp(compiler, instruction)
  result = emitByte(compiler, arg)

# Emits one bytecode instruction followed by a 16-bit argument, which will be
# written big endian.
proc emitShortArg(compiler: Compiler, instruction: Code, arg: int) =
  emitOp(compiler, instruction)
  emitShort(compiler, arg)

# Emits [instruction] followed by a placeholder for a jump offset. The
# placeholder can be patched by calling [jumpPatch]. Returns the index of the
# placeholder.
proc emitJump(compiler: Compiler, instruction: Code): int =
  emitOp(compiler, instruction)
  discard emitByte(compiler, 0xff)
  result = emitByte(compiler, 0xff) - 1

# Creates a new constant for the current value and emits the bytecode to load
# it from the constant table.
proc emitConstant(compiler: Compiler, value: Value) =
  let constant = addConstant(compiler, value)
  
  # Compile the code to load the constant.
  emitShortArg(compiler, CODE_CONSTANT, constant)

# Create a new local variable with [name]. Assumes the current scope is local
# and the name is unique.
proc addLocal(compiler: Compiler, name: cstring, length: int): int =
  let local = addr compiler.locals[compiler.numLocals]
  local.name = name
  local.length = length
  local.depth = compiler.scopeDepth
  local.isUpvalue = false
  result = compiler.numLocals
  inc compiler.numLocals

# Declares a variable in the current scope whose name is the given token.
#
# If [token] is `NULL`, uses the previously consumed token. Returns its symbol.
proc declareVariable(compiler: Compiler, tok: ptr Token): int =
  var token = tok
  if token == nil:
     token = addr compiler.parser.previous

  if token.length > MaxVariableName:
    error(compiler, "Variable name cannot be longer than %d characters.")

  # Top-level module scope.
  if compiler.scopeDepth == -1:
    let symbol = defineVariable(compiler.parser.vm,
                                    compiler.parser.module,
                                    token.start, token.length, NullVal)

    if symbol == -1:
      error(compiler, "Module variable is already defined.")
    elif symbol == -2:
      error(compiler, "Too many module variables defined.")

    return symbol

  # See if there is already a variable with this name declared in the current
  # scope. (Outer scopes are OK: those get shadowed.)
  var i = compiler.numLocals - 1;
  while i >= 0:
    let local = addr compiler.locals[i]

    # Once we escape this scope and hit an outer one, we can stop.
    if local.depth < compiler.scopeDepth:
      break

    if local.length == token.length and
        equalMem(local.name, token.start, token.length):
      error(compiler, "Variable is already declared in this scope.")
      return i

    dec i

  if compiler.numLocals == MAX_LOCALS:
    error(compiler, "Cannot declare more than %d variables in one scope.")
    return -1

  result = addLocal(compiler, token.start, token.length)

# Parses a name token and declares a variable in the current scope with that
# name. Returns its slot.
proc declareNamedVariable(compiler: Compiler): int =
  consume(compiler, TOKEN_NAME, "Expect variable name.")
  result = declareVariable(compiler, nil)

# Stores a variable with the previously defined symbol in the current scope.
proc defineVariable(compiler: Compiler, symbol: int) =
  # Store the variable. If it's a local, the result of the initializer is
  # in the correct slot on the stack already so we're done.
  if compiler.scopeDepth >= 0: return

  # It's a module-level variable, so store the value in the module slot and
  # then discard the temporary for the initializer.
  emitShortArg(compiler, CODE_STORE_MODULE_VAR, symbol)
  emitOp(compiler, CODE_POP)

# Starts a new local block scope.
proc pushScope(compiler: Compiler) =
  inc compiler.scopeDepth

# Generates code to discard local variables at [depth] or greater. Does *not*
# actually undeclare variables or pop any scopes, though. This is called
# directly when compiling "break" statements to ditch the local variables
# before jumping out of the loop even though they are still in scope *past*
# the break instruction.
#
# Returns the number of local variables that were eliminated.
proc discardLocals(compiler: Compiler, depth: int): int =
  assert(compiler.scopeDepth > -1, "Cannot exit top-level scope.")

  var local = compiler.numLocals - 1
  while local >= 0 and compiler.locals[local].depth >= depth:
    # If the local was closed over, make sure the upvalue gets closed when it
    # goes out of scope on the stack. We use emitByte() and not emitOp() here
    # because we don't want to track that stack effect of these pops since the
    # variables are still in scope after the break.
    if compiler.locals[local].isUpvalue:
      discard emitByte(compiler, CODE_CLOSE_UPVALUE.int)
    else:
      discard emitByte(compiler, CODE_POP.int)
    
    dec local

  result = compiler.numLocals - local - 1

# Closes the last pushed block scope and discards any local variables declared
# in that scope. This should only be called in a statement context where no
# temporaries are still on the stack.
proc popScope(compiler: Compiler) =
  let popped = discardLocals(compiler, compiler.scopeDepth)
  dec compiler.numLocals, popped
  dec compiler.numSlots, popped
  dec compiler.scopeDepth

# Attempts to look up the name in the local variables of [compiler]. If found,
# returns its index, otherwise returns -1.
proc resolveLocal(compiler: Compiler, name: cstring, length: int): int =
  # Look it up in the local scopes. Look in reverse order so that the most
  # nested variable is found first and shadows outer ones.
  result = compiler.numLocals - 1
  while result >= 0:
    if compiler.locals[result].length == length and
        equalMem(name, addr compiler.locals[result].name[0], length):
      break
    dec result

# Adds an upvalue to [compiler]'s function with the given properties. Does not
# add one if an upvalue for that variable is already in the list. Returns the
# index of the upvalue.
proc addUpvalue(compiler: Compiler, isLocal: bool, index: int): int =
  # Look for an existing one.
  for i in 0..<compiler.fn.numUpvalues:
    let upvalue = addr compiler.upvalues[i]
    if upvalue.index == index and upvalue.isLocal == isLocal:
      return i

  # If we got here, it's a new upvalue.
  compiler.upvalues[compiler.fn.numUpvalues].isLocal = isLocal
  compiler.upvalues[compiler.fn.numUpvalues].index = index
  result = compiler.fn.numUpvalues
  inc compiler.fn.numUpvalues

# Attempts to look up [name] in the functions enclosing the one being compiled
# by [compiler]. If found, it adds an upvalue for it to this compiler's list
# of upvalues (unless it's already in there) and returns its index. If not
# found, returns -1.
#
# If the name is found outside of the immediately enclosing function, this
# will flatten the closure and add upvalues to all of the intermediate
# functions so that it gets walked down to this one.
#
# If it reaches a method boundary, this stops and returns -1 since methods do
# not close over local variables.
proc findUpvalue(compiler: Compiler, name: cstring, length: int): int =
  # If we are at the top level, we didn't find it.
  if compiler.parent == nil:
    return -1
  
  # If we hit the method boundary (and the name isn't a static field), then
  # stop looking for it. We'll instead treat it as a self send.
  if name[0] != '_' and compiler.parent.enclosingClass != nil:
    return -1
  
  # See if it's a local variable in the immediately enclosing function.
  let local = resolveLocal(compiler.parent, name, length)
  if local != -1:
    # Mark the local as an upvalue so we know to close it when it goes out of
    # scope.
    compiler.parent.locals[local].isUpvalue = true

    return addUpvalue(compiler, true, local)

  # See if it's an upvalue in the immediately enclosing function. In other
  # words, if it's a local variable in a non-immediately enclosing function.
  # This "flattens" closures automatically: it adds upvalues to all of the
  # intermediate functions to get from the function where a local is declared
  # all the way into the possibly deeply nested function that is closing over
  # it.
  let upvalue = findUpvalue(compiler.parent, name, length)
  if upvalue != -1:
    return addUpvalue(compiler, false, upvalue)

  # If we got here, we walked all the way up the parent chain and couldn't
  # find it.
  result = -1

# Look up [name] in the current scope to see what variable it refers to.
# Returns the variable either in local scope, or the enclosing function's
# upvalue list. Does not search the module scope. Returns a variable with
# index -1 if not found.
proc resolveNonmodule(compiler: Compiler, name: cstring, length: int): Variable =
  # Look it up in the local scopes.
  result.scope = SCOPE_LOCAL
  result.index = resolveLocal(compiler, name, length)
  if result.index == -1:
  # It's not a local, so guess that it's an upvalue.
    result.scope = SCOPE_UPVALUE
    result.index = findUpvalue(compiler, name, length)

# Look up [name] in the current scope to see what variable it refers to.
# Returns the variable either in module scope, local scope, or the enclosing
# function's upvalue list. Returns a variable with index -1 if not found.
proc resolveName(compiler: Compiler, name: cstring, length: int): Variable =
  result = resolveNonmodule(compiler, name, length)
  if result.index == -1:
    result.scope = SCOPE_MODULE
    result.index = 
      find(compiler.parser.module.variableNames, name, length)

proc loadLocal(compiler: Compiler, slot: int) =
  if slot <= 8:
    emitOp(compiler, (Code)(CODE_LOAD_LOCAL_0.int + slot))
  else:
    discard emitByteArg(compiler, CODE_LOAD_LOCAL, slot)

# Finishes [compiler], which is compiling a function, method, or chunk of top
# level code. If there is a parent compiler, then this emits code in the
# parent compiler to load the resulting function.
proc endCompiler(compiler: Compiler,
                  debugName: cstring, debugNameLength: int): ObjFn =
  # If we hit an error, don't finish the function since it's borked anyway.
  if compiler.parser.hasError:
    compiler.parser.vm.compiler = compiler.parent
    return nil

  # Mark the end of the bytecode. Since it may contain multiple early returns,
  # we can't rely on CODE_RETURN to tell us we're at the end.
  emitOp(compiler, CODE_END)

  bindName(compiler.parser.vm, compiler.fn,
                       debugName, debugNameLength)
  
  # In the function that contains this one, load the resulting function object.
  if compiler.parent != nil:
    let constant = addConstant(compiler.parent, compiler.fn.val)

    # Wrap the function in a closure. We do this even if it has no upvalues so
    # that the VM can uniformly assume all called objects are closures. This
    # makes creating a function a little slower, but makes invoking them
    # faster. Given that functions are invoked more often than they are
    # created, this is a win.
    emitShortArg(compiler.parent, CODE_CLOSURE, constant)

    # Emit arguments for each upvalue to know whether to capture a local or
    # an upvalue.
    for i in 0..<compiler.fn.numUpvalues:
      discard emitByte(compiler.parent, if compiler.upvalues[i].isLocal: 1 else: 0)
      discard emitByte(compiler.parent, compiler.upvalues[i].index)

  # Pop this compiler off the stack.
  compiler.parser.vm.compiler = compiler.parent
  
  when true: #defined(DebugDumpCompiledCode):
    dumpCode(compiler.parser.vm, compiler.fn)

  result = compiler.fn

##
## Grammar ---------------------------
##

type
  Precedence = enum
    PREC_NONE,
    PREC_LOWEST,
    PREC_ASSIGNMENT,    # =
    PREC_CONDITIONAL,   # ?:
    PREC_LOGICAL_OR,    # ||
    PREC_LOGICAL_AND,   # &&
    PREC_EQUALITY,      # == !=
    PREC_IS,            # is
    PREC_COMPARISON,    # < > <= >=
    PREC_BITWISE_OR,    # |
    PREC_BITWISE_XOR,   # ^
    PREC_BITWISE_AND,   # &
    PREC_BITWISE_SHIFT, # << >>
    PREC_RANGE,         # .. ...
    PREC_TERM,          # + -
    PREC_FACTOR,        # * / %
    PREC_UNARY,         # unary - ! ~
    PREC_CALL,          # . () []
    PREC_PRIMARY

  GrammarFn = proc(compiler: Compiler, canAssign: bool) {.nimcall.}
  SignatureFn = proc(compiler: Compiler, signature: var Signature) {.nimcall.}

  GrammarRule = object
    prefix: GrammarFn
    infix: GrammarFn
    meth: SignatureFn
    precedence: Precedence
    name: cstring

# Forward declarations since the grammar is recursive.
proc getRulePrecedence(kind: TokenKind): Precedence
proc getRuleName(kind: TokenKind): cstring

proc expression(compiler: Compiler)
proc statement(compiler: Compiler)
proc definition(compiler: Compiler)
proc parsePrecedence(compiler: Compiler, precedence: Precedence)

# Replaces the placeholder argument for a previous CODE_JUMP or CODE_JUMP_IF
# instruction with an offset that jumps to the current end of bytecode.
proc patchJump(compiler: Compiler, offset: int) =
  # -2 to adjust for the bytecode for the jump offset itself.
  let jump = compiler.fn.code.len - offset - 2
  if jump > MAX_JUMP:
    error(compiler, "Too much code to jump over.")

  compiler.fn.code.data[offset] = (jump shr 8) and 0xff
  compiler.fn.code.data[offset + 1] = jump and 0xff

# Parses a block body, after the initial "{" has been consumed.
#
# Returns true if it was a expression body, false if it was a statement body.
# (More precisely, returns true if a value was left on the stack. An empty
# block returns false.)
proc finishBlock(compiler: Compiler): bool =
  # Empty blocks do nothing.
  if match(compiler, TOKEN_RIGHT_BRACE):
    return false

  # If there's no line after the "{", it's a single-expression body.
  if not matchLine(compiler):
    expression(compiler)
    consume(compiler, TOKEN_RIGHT_BRACE, "Expect '}' at end of block.")
    return true

  # Empty blocks (with just a newline inside) do nothing.
  if match(compiler, TOKEN_RIGHT_BRACE):
    return false

  # Compile the definition list.
  while true:
    definition(compiler)

    # If we got into a weird error state, don't get stuck in a loop.
    if peek(compiler) == TOKEN_EOF:
      return true

    consumeLine(compiler, "Expect newline after statement.")
    if match(compiler, TOKEN_RIGHT_BRACE):
      break
  result = false

# Parses a method or function body, after the initial "{" has been consumed.
#
# It [isInitializer] is `true`, this is the body of a constructor initializer.
# In that case, this adds the code to ensure it returns `this`.
proc finishBody(compiler: Compiler, isInitializer: bool) =
  let isExpressionBody = finishBlock(compiler)

  if isInitializer:
    # If the initializer body evaluates to a value, discard it.
    if isExpressionBody: 
      emitOp(compiler, CODE_POP)

    # The receiver is always stored in the first local slot.
    emitOp(compiler, CODE_LOAD_LOCAL_0)
  elif not isExpressionBody:
    # Implicitly return null in statement bodies.
    emitOp(compiler, CODE_NULL)

  emitOp(compiler, CODE_RETURN)

# The VM can only handle a certain number of parameters, so check that we
# haven't exceeded that and give a usable error.
proc validateNumParameters(compiler: Compiler, numArgs: int) =
  if numArgs == MaxParameters + 1:
    # Only show an error at exactly max + 1 so that we can keep parsing the
    # parameters and minimize cascaded errors.
    error(compiler, "Methods cannot have more than %d parameters.")

# Parses the rest of a comma-separated parameter list after the opening
# delimeter. Updates `arity` in [signature] with the number of parameters.
proc finishParameterList(compiler: Compiler, signature: var Signature) =
  while true:
    ignoreNewlines(compiler)
    inc signature.arity
    validateNumParameters(compiler, signature.arity)

    # Define a local variable in the method for the parameter.
    discard declareNamedVariable(compiler)
    if not match(compiler, TOKEN_COMMA):
      break

# Gets the symbol for a method [name] with [length].
proc methodSymbol(compiler: Compiler, name: cstring, length: int): int =
  compiler.parser.vm.ensure(compiler.parser.vm.methodNames, name, length)

# Appends characters to [name] (and updates [length]) for [numParams] "_"
# surrounded by [leftBracket] and [rightBracket].
proc signatureParameterList(name: var openarray[char], pos: int,
                            numParams: int, leftBracket, rightBracket: char): int =
  result = pos
  name[result] = leftBracket
  inc result
  for i in 0..<numParams:
    if i > 0: 
      name[result] = ','
      inc result
    name[result] = '_'
    inc result
  name[result] = rightBracket
  inc result

# Fills [name] with the stringified version of [signature] and updates
# [length] to the resulting length.
proc signatureToString(signature: Signature,
                       name: var openarray[char]): int =
  result = 0

  # Build the full name from the signature.
  copyMem(addr name[result], signature.name, signature.length)
  inc result, signature.length

  case signature.kind:
  of skMethod:
    result = signatureParameterList(name, result, signature.arity, '(', ')')
  
  of skGetter:
      # The signature is just the name.
      discard
  
  of skSetter:
    name[result] = '='
    inc result
    result = signatureParameterList(name, result, 1, '(', ')')

  of skSubscript:
    result = signatureParameterList(name, result, signature.arity, '[', ']')

  of skSubscriptSetter:
      result = signatureParameterList(name, result, signature.arity - 1, '[', ']')
      name[result] = '='
      inc result
      result = signatureParameterList(name, result, 1, '(', ')')
      
  of skInitializer:
      copyMem(addr name[0], cstring("init "), 5)
      copyMem(addr name[5], signature.name, signature.length)
      result = 5 + signature.length
      result = signatureParameterList(name, result, signature.arity, '(', ')')

  name[result] = char(0)

# Gets the symbol for a method with [signature].
proc signatureSymbol(compiler: Compiler, signature: Signature): int =
  # Build the full name from the signature.
  var name: array[MAX_METHOD_SIGNATURE, char]
  let length = signatureToString(signature, name)

  result = methodSymbol(compiler, cstring(addr name[0]), length)

# Returns a signature with [type] whose name is from the last consumed token.
proc signatureFromToken(compiler: Compiler, signature: var Signature, kind: SignatureKind) =  
  # Get the token for the method name.
  let token = addr compiler.parser.previous
  signature.name = token.start
  signature.length = token.length
  signature.kind = kind
  signature.arity = 0;

  if signature.length > MaxMethodName:
    error(compiler, "Method names cannot be longer than %d characters.")
    signature.length = MAX_METHOD_NAME

# Parses a comma-separated list of arguments. Modifies [signature] to include
# the arity of the argument list.
proc finishArgumentList(compiler: Compiler, signature: var Signature) =
  while true:
    ignoreNewlines(compiler)
    inc signature.arity
    validateNumParameters(compiler, signature.arity)
    expression(compiler)
    if not match(compiler, TOKEN_COMMA):
      break

  # Allow a newline before the closing delimiter.
  ignoreNewlines(compiler)

proc callSignature(compiler: Compiler, instruction: Code,
      signature: Signature) =
  let symbol = signatureSymbol(compiler, signature)
  emitShortArg(compiler, (Code)(instruction.int + signature.arity), symbol)

  if instruction == CODE_SUPER_0:
    # Super calls need to be statically bound to the class's superclass. This
    # ensures we call the right method even when a method containing a super
    # call is inherited by another subclass.
    #
    # We bind it at class definition time by storing a reference to the
    # superclass in a constant. So, here, we create a slot in the constant
    # table and store NULL in it. When the method is bound, we'll look up the
    # superclass then and store it in the constant slot.
    emitShort(compiler, addConstant(compiler, NullVal))

# Compiles a method call with [numArgs] for a method with [name] with [length].
proc callMethod(compiler: Compiler, numArgs: int, name: cstring, length: int) =
  let symbol = methodSymbol(compiler, name, length)
  emitShortArg(compiler, (Code)(CODE_CALL_0.int + numArgs), symbol)

proc init(signature: var Signature, name: cstring, length: int, kind: SignatureKind, arity: int) =
  signature.name = name
  signature.length = length
  signature.kind = kind
  signature.arity = arity

# Compiles an (optional) argument list for a method call with [methodSignature]
# and then calls it.
proc methodCall(compiler: Compiler, instruction: Code,
                       signature: Signature) =
  #echo "method call: ", repr signature.name
  # Make a new signature that contains the updated arity and type based on
  # the arguments we find.
  var called: Signature
  called.init(signature.name, signature.length, skGetter, 0)

  # Parse the argument list, if any.
  if match(compiler, TOKEN_LEFT_PAREN):
    called.kind = skMethod

    # Allow empty an argument list.
    if peek(compiler) != TOKEN_RIGHT_PAREN:
      finishArgumentList(compiler, called)
    consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.")

  # Parse the block argument, if any.
  if match(compiler, TOKEN_LEFT_BRACE):
    # Include the block argument in the arity.
    called.kind = skMethod
    inc called.arity

    var fnCompiler: WrenCompiler
    init(fnCompiler, compiler.parser, compiler, true)

    # Make a dummy signature to track the arity.
    var fnSignature: Signature
    fnSignature.init("", 0, skMethod, 0)

    # Parse the parameter list, if any.
    if match(compiler, TOKEN_PIPE):
      finishParameterList(addr fnCompiler, fnSignature)
      consume(compiler, TOKEN_PIPE, "Expect '|' after function parameters.")

    fnCompiler.fn.arity = fnSignature.arity

    finishBody(addr fnCompiler, false)

    # Name the function based on the method its passed to.
    var 
      blockName: array[MAX_METHOD_SIGNATURE + 15, char]
    let blockLength = signatureToString(called, blockName)
    copyMem(addr blockName[blockLength], cstring(" block argument"), 16)

    discard endCompiler(addr fnCompiler, blockName, blockLength + 15)

  # TODO: Allow Grace-style mixfix methods?

  # If this is a super() call for an initializer, make sure we got an actual
  # argument list.
  if signature.kind == skInitializer:
    if called.kind != skMethod:
      error(compiler, "A superclass constructor must have an argument list.")
    
    called.kind = skInitializer
  
  callSignature(compiler, instruction, called)

# Compiles a call whose name is the previously consumed token. This includes
# getters, method calls with arguments, and setter calls.
proc namedCall(compiler: Compiler, canAssign: bool, instruction: Code) =
  # Get the token for the method name.
  var signature: Signature
  signatureFromToken(compiler, signature, skGetter)

  if canAssign and match(compiler, TOKEN_EQ):
    ignoreNewlines(compiler)

    # Build the setter signature.
    signature.kind = skSetter
    signature.arity = 1

    # Compile the assigned value.
    expression(compiler)
    callSignature(compiler, instruction, signature)
  else:
    methodCall(compiler, instruction, signature)

# Emits the code to load [variable] onto the stack.
proc loadVariable(compiler: Compiler, variable: Variable) =
  case variable.scope:
  of SCOPE_LOCAL:
    loadLocal(compiler, variable.index)
  of SCOPE_UPVALUE:
    discard emitByteArg(compiler, CODE_LOAD_UPVALUE, variable.index)
  of SCOPE_MODULE:
    emitShortArg(compiler, CODE_LOAD_MODULE_VAR, variable.index)

# Loads the receiver of the currently enclosing method. Correctly handles
# functions defined inside methods.
proc loadThis(compiler: Compiler) =
  loadVariable(compiler, resolveNonmodule(compiler, "this", 4))

# Pushes the value for a module-level variable implicitly imported from core.
proc loadCoreVariable(compiler: Compiler, name: cstring) =
  var symbol = find(compiler.parser.module.variableNames, name, name.len)
  assert(symbol != -1, "Should have already defined core name.")
  emitShortArg(compiler, CODE_LOAD_MODULE_VAR, symbol)

# A parenthesized expression.
proc grouping(compiler: Compiler, canAssign: bool) =
  expression(compiler)
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after expression.")

# A list literal.
proc list(compiler: Compiler, canAssign: bool) =
  # Instantiate a new list.
  loadCoreVariable(compiler, "List")
  callMethod(compiler, 0, "new()", 5);
  
  # Compile the list elements. Each one compiles to a ".add()" call.
  while true:
    ignoreNewlines(compiler)

    # Stop if we hit the end of the list.
    if peek(compiler) == TOKEN_RIGHT_BRACKET:
      break

    # The element.
    expression(compiler)
    callMethod(compiler, 1, "addCore_(_)", 11)
    if not (match(compiler, TOKEN_COMMA)):
      break

  # Allow newlines before the closing ']'.
  ignoreNewlines(compiler)
  consume(compiler, TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.")

# A map literal.
proc map(compiler: Compiler, canAssign: bool) =
  # Instantiate a new map.
  loadCoreVariable(compiler, "Map")
  callMethod(compiler, 0, "new()", 5)

  # Compile the map elements. Each one is compiled to just invoke the
  # subscript setter on the map.
  while true:
    ignoreNewlines(compiler)

    # Stop if we hit the end of the map.
    if peek(compiler) == TOKEN_RIGHT_BRACE:
      break

    # The key.
    parsePrecedence(compiler, PREC_UNARY)
    consume(compiler, TOKEN_COLON, "Expect ':' after map key.")
    ignoreNewlines(compiler)

    # The value.
    expression(compiler)
    callMethod(compiler, 2, "addCore_(_,_)", 13)
    if not match(compiler, TOKEN_COMMA):
      break

  # Allow newlines before the closing '}'.
  ignoreNewlines(compiler)
  consume(compiler, TOKEN_RIGHT_BRACE, "Expect '}' after map entries.")

# Unary operators like `-foo`.
proc unaryOp(compiler: Compiler, canAssign: bool) =
  let name = getRuleName(compiler.parser.previous.kind)

  ignoreNewlines(compiler)

  # Compile the argument.
  parsePrecedence(compiler, (Precedence)(PREC_UNARY.int + 1))

  # Call the operator method on the left-hand side.
  callMethod(compiler, 0, name, 1);

proc boolean(compiler: Compiler, canAssign: bool) =
  emitOp(compiler,
    if compiler.parser.previous.kind == TOKEN_FALSE: CODE_FALSE else: CODE_TRUE)

# Walks the compiler chain to find the compiler for the nearest class
# enclosing this one. Returns NULL if not currently inside a class definition.
proc getEnclosingClassCompiler(compiler: Compiler): Compiler =
  result = compiler
  while result != nil:
    if result.enclosingClass != nil:
      break
    result = result.parent

# Walks the compiler chain to find the nearest class enclosing this one.
# Returns NULL if not currently inside a class definition.
proc getEnclosingClass(compiler: Compiler): ptr ClassCompiler =
  let compiler = getEnclosingClassCompiler(compiler)
  if compiler != nil: 
    result = compiler.enclosingClass

proc field(compiler: Compiler, canAssign: bool) =
  # Initialize it with a fake value so we can keep parsing and minimize the
  # number of cascaded errors.
  var field = 255

  let enclosingClass = getEnclosingClass(compiler)

  if enclosingClass == nil:
    error(compiler, "Cannot reference a field outside of a class definition.")
  elif enclosingClass.isForeign:
    error(compiler, "Cannot define fields in a foreign class.")
  elif enclosingClass.inStatic:
    error(compiler, "Cannot use an instance field in a static method.")
  else:
    # Look up the field, or implicitly define it.
    field = ensure(compiler.parser.vm, enclosingClass.fields,
        compiler.parser.previous.start,
        compiler.parser.previous.length)

    if field >= MaxFields:
      error(compiler, "A class can only have %d fields.")

  # If there's an "=" after a field name, it's an assignment.
  var isLoad = true
  if canAssign and match(compiler, TOKEN_EQ):
    # Compile the right-hand side.
    expression(compiler)
    isLoad = false

  # If we're directly inside a method, use a more optimal instruction.
  if compiler.parent != nil and
      compiler.parent.enclosingClass == enclosingClass:
    discard emitByteArg(compiler, 
      if isLoad: CODE_LOAD_FIELD_THIS else: CODE_STORE_FIELD_THIS, field)
  else:
    loadThis(compiler)
    discard emitByteArg(compiler, 
      if isLoad: CODE_LOAD_FIELD else: CODE_STORE_FIELD, field)

# Compiles a read or assignment to [variable].
proc bareName(compiler: Compiler, canAssign: bool, variable: Variable) =
  # If there's an "=" after a bare name, it's a variable assignment.
  if canAssign and match(compiler, TOKEN_EQ):
    # Compile the right-hand side.
    expression(compiler)

    # Emit the store instruction.
    case variable.scope:
    of SCOPE_LOCAL:
      discard emitByteArg(compiler, CODE_STORE_LOCAL, variable.index)
    of SCOPE_UPVALUE:
      discard emitByteArg(compiler, CODE_STORE_UPVALUE, variable.index)
    of SCOPE_MODULE:
      emitShortArg(compiler, CODE_STORE_MODULE_VAR, variable.index)
  else:
    # Emit the load instruction.
    loadVariable(compiler, variable)

proc staticField(compiler: Compiler, canAssign: bool) =
  let classCompiler = getEnclosingClassCompiler(compiler)
  if classCompiler == nil:
    error(compiler, "Cannot use a static field outside of a class definition.")
    return

  # Look up the name in the scope chain.
  let token = addr compiler.parser.previous

  # If this is the first time we've seen this static field, implicitly
  # define it as a variable in the scope surrounding the class definition.
  if resolveLocal(classCompiler, token.start, token.length) == -1:
    let symbol = declareVariable(classCompiler, nil)

    # Implicitly initialize it to null.
    emitOp(classCompiler, CODE_NULL)
    defineVariable(classCompiler, symbol)

  # It definitely exists now, so resolve it properly. This is different from
  # the above resolveLocal() call because we may have already closed over it
  # as an upvalue.
  let variable = resolveName(compiler, token.start, token.length)
  bareName(compiler, canAssign, variable)

# Returns `true` if [name] is a local variable name (starts with a lowercase
# letter).
proc isLocalName(name: cstring): bool = name[0] >= 'a' and name[0] <= 'z'

# Compiles a variable name or method call with an implicit receiver.
proc name(compiler: Compiler, canAssign: bool) =
  # Look for the name in the scope chain up to the nearest enclosing method.
  let token = addr compiler.parser.previous

  var variable = resolveNonmodule(compiler, token.start, token.length)
  if variable.index != -1:
    bareName(compiler, canAssign, variable)
    return;

  # TODO: The fact that we return above here if the variable is known and parse
  # an optional argument list below if not means that the grammar is not
  # context-free. A line of code in a method like "someName(foo)" is a parse
  # error if "someName" is a defined variable in the surrounding scope and not
  # if it isn't. Fix this. One option is to have "someName(foo)" always
  # resolve to a self-call if there is an argument list, but that makes
  # getters a little confusing.

  # If we're inside a method and the name is lowercase, treat it as a method
  # on this.
  if isLocalName(token.start) and getEnclosingClass(compiler) != nil:
    loadThis(compiler)
    namedCall(compiler, canAssign, CODE_CALL_0)
    return

  # Otherwise, look for a module-level variable with the name.
  variable.scope = SCOPE_MODULE
  variable.index = find(compiler.parser.module.variableNames,
                                       token.start, token.length)
  if variable.index == -1:
    if isLocalName(token.start):
      error(compiler, "Undefined variable.")
      return

    # If it's a nonlocal name, implicitly define a module-level variable in
    # the hopes that we get a real definition later.
    variable.index = declareVariable(compiler.parser.vm,
                                         compiler.parser.module,
                                         token.start, token.length,
                                         token.line)

    if variable.index == -2:
      error(compiler, "Too many module variables defined.")
  
  bareName(compiler, canAssign, variable)

proc null(compiler: Compiler, canAssign: bool) =
  emitOp(compiler, CODE_NULL)

# A number or string literal.
proc literal(compiler: Compiler, canAssign: bool) =
  emitConstant(compiler, compiler.parser.previous.value)

proc infixOp(compiler: Compiler, canAssign: bool) =
  let name = getRuleName(compiler.parser.previous.kind)

  # An infix operator cannot end an expression.
  ignoreNewlines(compiler)

  # Compile the right-hand side.
  parsePrecedence(compiler, 
    (Precedence)(getRulePrecedence(compiler.parser.previous.kind).int + 1))

  # Call the operator method on the left-hand side.
  var signature: Signature
  signature.init(name, name.len, skMethod, 1)
  callSignature(compiler, CODE_CALL_0, signature)

# A string literal that contains interpolated expressions.
#
# Interpolation is syntactic sugar for calling ".join()" on a list. So the
# string:
#
#     "a %(b + c) d"
#
# is compiled roughly like:
#
#     ["a ", b + c, " d"].join()
proc stringInterpolation(compiler: Compiler, canAssign: bool) =
  # Instantiate a new list.
  loadCoreVariable(compiler, "List")
  callMethod(compiler, 0, "new()", 5)
  
  while true:
    # The opening string part.
    literal(compiler, false)
    callMethod(compiler, 1, "addCore_(_)", 11)
    
    # The interpolated expression.
    ignoreNewlines(compiler)
    expression(compiler)
    callMethod(compiler, 1, "addCore_(_)", 11)
    
    ignoreNewlines(compiler)
    if not match(compiler, TOKEN_INTERPOLATION):
      break
  
  # The trailing string part.
  consume(compiler, TOKEN_STRING, "Expect end of string interpolation.")
  literal(compiler, false)
  callMethod(compiler, 1, "addCore_(_)", 11)
  
  # The list of interpolated parts.
  callMethod(compiler, 0, "join()", 6)

proc super(compiler: Compiler, canAssign: bool) =
  let enclosingClass = getEnclosingClass(compiler)

  if enclosingClass == nil:
    error(compiler, "Cannot use 'super' outside of a method.")

  loadThis(compiler)

  # TODO: Super operator calls.
  # TODO: There's no syntax for invoking a superclass constructor with a
  # different name from the enclosing one. Figure that out.

  # See if it's a named super call, or an unnamed one.
  if match(compiler, TOKEN_DOT):
    # Compile the superclass call.
    consume(compiler, TOKEN_NAME, "Expect method name after 'super.'.")
    namedCall(compiler, canAssign, CODE_SUPER_0)
  elif enclosingClass != nil:
    # No explicit name, so use the name of the enclosing method. Make sure we
    # check that enclosingClass isn't NULL first. We've already reported the
    # error, but we don't want to crash here.
    methodCall(compiler, CODE_SUPER_0, enclosingClass.signature[])

# Compiles a method signature for an infix operator.
proc infixSignature(compiler: Compiler, signature: var Signature) =
  # Add the RHS parameter.
  signature.kind = skMethod
  signature.arity = 1

  # Parse the parameter name.
  consume(compiler, TOKEN_LEFT_PAREN, "Expect '(' after operator name.")
  discard declareNamedVariable(compiler)
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.")

# Compiles a method signature for an unary operator (i.e. "!").
proc unarySignature(compiler: Compiler, signature: var Signature) =
  # Do nothing. The name is already complete.
  signature.kind = skGetter

# Compiles a method signature for an operator that can either be unary or
# infix (i.e. "-").
proc mixedSignature(compiler: Compiler, signature: var Signature) =
  signature.kind = skGetter

  # If there is a parameter, it's an infix operator, otherwise it's unary.
  if match(compiler, TOKEN_LEFT_PAREN):
    # Add the RHS parameter.
    signature.kind = skMethod
    signature.arity = 1

    # Parse the parameter name.
    discard declareNamedVariable(compiler)
    consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.")

# Compiles an optional setter parameter in a method [signature].
#
# Returns `true` if it was a setter.
proc maybeSetter(compiler: Compiler, signature: var Signature): bool =
  # See if it's a setter.
  if match(compiler, TOKEN_EQ):
    
    # It's a setter.
    if signature.kind == skSubscript:
      signature.kind = skSubscriptSetter
    else:
      signature.kind = skSetter

    # Parse the value parameter.
    consume(compiler, TOKEN_LEFT_PAREN, "Expect '(' after '='.")
    discard declareNamedVariable(compiler)
    consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.")

    inc signature.arity
    result = true

# Compiles a method signature for a subscript operator.
proc subscriptSignature(compiler: Compiler, signature: var Signature) =
  signature.kind = skSubscript

  # The signature currently has "[" as its name since that was the token that
  # matched it. Clear that out.
  signature.length = 0

  # Parse the parameters inside the subscript.
  finishParameterList(compiler, signature)
  consume(compiler, TOKEN_RIGHT_BRACKET, "Expect ']' after parameters.")

  discard maybeSetter(compiler, signature)

# Parses an optional parenthesized parameter list. Updates `type` and `arity`
# in [signature] to match what was parsed.
proc parameterList(compiler: Compiler, signature: var Signature) =
  # The parameter list is optional.
  if match(compiler, TOKEN_LEFT_PAREN):  
    signature.kind = skMethod
    
    # Allow an empty parameter list.
    if not match(compiler, TOKEN_RIGHT_PAREN):
      finishParameterList(compiler, signature)
      consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.")

# Compiles a method signature for a named method or setter.
proc namedSignature(compiler: Compiler, signature: var Signature) =
  signature.kind = skGetter
  
  # If it's a setter, it can't also have a parameter list.
  if not maybeSetter(compiler, signature):
    # Regular named method with an optional parameter list.
    parameterList(compiler, signature)

# Compiles a method signature for a constructor.
proc constructorSignature(compiler: Compiler, signature: var Signature) =
  consume(compiler, TOKEN_NAME, "Expect constructor name after 'construct'.")
  
  # Capture the name.
  signatureFromToken(compiler, signature, skInitializer)
  
  if match(compiler, TOKEN_EQ):
    error(compiler, "A constructor cannot be a setter.")

  if not match(compiler, TOKEN_LEFT_PAREN):
    error(compiler, "A constructor cannot be a getter.")
  else:
    # Allow an empty parameter list.
    if not match(compiler, TOKEN_RIGHT_PAREN):
      finishParameterList(compiler, signature);
      consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.")

proc this(compiler: Compiler, canAssign: bool) =
  if getEnclosingClass(compiler) == nil:
    error(compiler, "Cannot use 'this' outside of a method.")
    return

  loadThis(compiler)

# Subscript or "array indexing" operator like `foo[bar]`.
proc subscript(compiler: Compiler, canAssign: bool) =
  var signature: Signature
  signature.name = ""
  signature.length = 0
  signature.kind = skSubscript
  signature.arity = 0

  # Parse the argument list.
  finishArgumentList(compiler, signature)
  consume(compiler, TOKEN_RIGHT_BRACKET, "Expect ']' after arguments.")

  if canAssign and match(compiler, TOKEN_EQ):
    signature.kind = skSubscriptSetter

    # Compile the assigned value.
    inc signature.arity
    validateNumParameters(compiler, signature.arity)
    expression(compiler)

  callSignature(compiler, CODE_CALL_0, signature)

proc call(compiler: Compiler, canAssign: bool) =
  ignoreNewlines(compiler)
  consume(compiler, TOKEN_NAME, "Expect method name after '.'.")
  namedCall(compiler, canAssign, CODE_CALL_0)

proc andOp(compiler: Compiler, canAssign: bool) =
  ignoreNewlines(compiler)

  # Skip the right argument if the left is false.
  let jump = emitJump(compiler, CODE_AND)
  parsePrecedence(compiler, PREC_LOGICAL_AND)
  patchJump(compiler, jump)

proc orOp(compiler: Compiler, canAssign: bool) =
  ignoreNewlines(compiler)

  # Skip the right argument if the left is true.
  let jump = emitJump(compiler, CODE_OR)
  parsePrecedence(compiler, PREC_LOGICAL_OR)
  patchJump(compiler, jump);

proc conditional(compiler: Compiler, canAssign: bool) =
  # Ignore newline after '?'.
  ignoreNewlines(compiler)

  # Jump to the else branch if the condition is false.
  let ifJump = emitJump(compiler, CODE_JUMP_IF)

  # Compile the then branch.
  parsePrecedence(compiler, PREC_CONDITIONAL)

  consume(compiler, TOKEN_COLON,
          "Expect ':' after then branch of conditional operator.")
  ignoreNewlines(compiler)

  # Jump over the else branch when the if branch is taken.
  let elseJump = emitJump(compiler, CODE_JUMP)

  # Compile the else branch.
  patchJump(compiler, ifJump)

  parsePrecedence(compiler, PREC_ASSIGNMENT)

  # Patch the jump over the else.
  patchJump(compiler, elseJump)

# This table defines all of the parsing rules for the prefix and infix
# expressions in the grammar. Expressions are parsed using a Pratt parser.
#
# See: http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
template RULE(p, i, m, pr, n): untyped = 
  GrammarRule(prefix: p, infix: i, meth: m, precedence: pr, name: n)  
template UNUSED(): untyped = 
  GrammarRule(prefix: nil, infix: nil, meth: nil, precedence: PREC_NONE, name: nil)  
template PREFIX(fn): untyped = 
  GrammarRule(prefix: fn, infix: nil, meth: nil, precedence: PREC_NONE, name: nil)  
template INFIX(prec, fn): untyped = 
  GrammarRule(prefix: nil, infix: fn, meth: nil, precedence: prec, name: nil)  
template INFIX_OPERATOR(prec, n): untyped = 
  GrammarRule(prefix: nil, infix: infixOp, meth: infixSignature, precedence: prec, name: n)
template PREFIX_OPERATOR(n): untyped = 
  GrammarRule(prefix: unaryOp, infix: nil, meth: unarySignature, precedence: PREC_NONE, name: n)  
template OPERATOR(n): untyped = 
  GrammarRule(prefix: unaryOp, infix: infixOp, meth: mixedSignature, precedence: PREC_TERM, name: n)  

const
  rules: array[TokenKind, GrammarRule] = [
    PREFIX(grouping), # /* TOKEN_LEFT_PAREN    */
    UNUSED, # /* TOKEN_RIGHT_PAREN   */ 
    RULE(list, subscript, subscriptSignature, PREC_CALL, nil), # /* TOKEN_LEFT_BRACKET  */
    UNUSED, # /* TOKEN_RIGHT_BRACKET */ 
    PREFIX(map), # /* TOKEN_LEFT_BRACE    */ 
    UNUSED, # /* TOKEN_RIGHT_BRACE   */
    UNUSED, # /* TOKEN_COLON         */ 
    INFIX(PREC_CALL, call), # /* TOKEN_DOT           */ 
    INFIX_OPERATOR(PREC_RANGE, ".."), # /* TOKEN_DOTDOT        */ 
    INFIX_OPERATOR(PREC_RANGE, "..."), # /* TOKEN_DOTDOTDOT     */ 
    UNUSED, # /* TOKEN_COMMA         */ 
    INFIX_OPERATOR(PREC_FACTOR, "*"), # /* TOKEN_STAR          */ 
    INFIX_OPERATOR(PREC_FACTOR, "/"), # /* TOKEN_SLASH         */ 
    INFIX_OPERATOR(PREC_FACTOR, "%"), # /* TOKEN_PERCENT       */ 
    INFIX_OPERATOR(PREC_TERM, "+"), # /* TOKEN_PLUS          */ 
    OPERATOR("-"), # /* TOKEN_MINUS         */ 
    INFIX_OPERATOR(PREC_BITWISE_SHIFT, "<<"), # /* TOKEN_LTLT          */ 
    INFIX_OPERATOR(PREC_BITWISE_SHIFT, ">>"), # /* TOKEN_GTGT          */ 
    INFIX_OPERATOR(PREC_BITWISE_OR, "|"), # /* TOKEN_PIPE          */ 
    INFIX(PREC_LOGICAL_OR, orOp), # /* TOKEN_PIPEPIPE      */ 
    INFIX_OPERATOR(PREC_BITWISE_XOR, "^"), # /* TOKEN_CARET         */ 
    INFIX_OPERATOR(PREC_BITWISE_AND, "&"), # /* TOKEN_AMP           */ 
    INFIX(PREC_LOGICAL_AND, andOp), # /* TOKEN_AMPAMP        */ 
    PREFIX_OPERATOR("!"), # /* TOKEN_BANG          */ 
    PREFIX_OPERATOR("~"),
    INFIX(PREC_ASSIGNMENT, conditional), #     /* TOKEN_QUESTION      */ 
    UNUSED, #     /* TOKEN_EQ            */ 
    INFIX_OPERATOR(PREC_COMPARISON, "<"), #     /* TOKEN_LT            */ 
    INFIX_OPERATOR(PREC_COMPARISON, ">"), #     /* TOKEN_GT            */ 
    INFIX_OPERATOR(PREC_COMPARISON, "<="), #     /* TOKEN_LTEQ          */ 
    INFIX_OPERATOR(PREC_COMPARISON, ">="), #     /* TOKEN_GTEQ          */ 
    INFIX_OPERATOR(PREC_EQUALITY, "=="), #     /* TOKEN_EQEQ          */ 
    INFIX_OPERATOR(PREC_EQUALITY, "!="), #     /* TOKEN_BANGEQ        */ 
    UNUSED, #     /* TOKEN_BREAK         */ 
    UNUSED, #     /* TOKEN_CLASS         */ 
    RULE( nil, nil, constructorSignature, PREC_NONE, nil ), #     /* TOKEN_CONSTRUCT     */ 
    UNUSED, #     /* TOKEN_ELSE          */ 
    PREFIX(boolean), #     /* TOKEN_FALSE         */ 
    UNUSED, #     /* TOKEN_FOR           */ 
    UNUSED, #     /* TOKEN_FOREIGN       */ 
    UNUSED, #     /* TOKEN_IF            */ 
    UNUSED, #     /* TOKEN_IMPORT        */ 
    UNUSED, #     /* TOKEN_IN            */ 
    INFIX_OPERATOR(PREC_IS, "is"), #     /* TOKEN_IS            */ 
    PREFIX(null), #     /* TOKEN_NULL          */ 
    UNUSED, #     /* TOKEN_RETURN        */ 
    UNUSED, #     /* TOKEN_STATIC        */ 
    PREFIX(super), #     /* TOKEN_SUPER         */ 
    PREFIX(this), #     /* TOKEN_THIS          */ 
    PREFIX(boolean), #     /* TOKEN_TRUE          */ 
    UNUSED, #     /* TOKEN_VAR           */ 
    UNUSED, #     /* TOKEN_WHILE         */ 
    PREFIX(field), #     /* TOKEN_FIELD         */ 
    PREFIX(staticField), #     /* TOKEN_STATIC_FIELD  */ 
    RULE(name, nil, namedSignature, PREC_NONE, nil ), #     /* TOKEN_NAME          */ 
    PREFIX(literal), #     /* TOKEN_NUMBER        */ 
    PREFIX(literal), #     /* TOKEN_STRING        */ 
    PREFIX(stringInterpolation), #     /* TOKEN_INTERPOLATION */ 
    UNUSED, #     /* TOKEN_LINE          */ 
    UNUSED, #     /* TOKEN_ERROR         */ 
    UNUSED #     /* TOKEN_EOF           */
  ] 

proc getRulePrecedence(kind: TokenKind): Precedence = rules[kind].precedence
proc getRuleName(kind: TokenKind): cstring = rules[kind].name

# The main entrypoint for the top-down operator precedence parser.
proc parsePrecedence(compiler: Compiler, precedence: Precedence) =
  nextToken(compiler.parser[])
  let prefix = rules[compiler.parser.previous.kind].prefix;

  if prefix == nil:
    error(compiler, "Expected expression.")
    return

  # Track if the precendence of the surrounding expression is low enough to
  # allow an assignment inside this one. We can't compile an assignment like
  # a normal expression because it requires us to handle the LHS specially --
  # it needs to be an lvalue, not an rvalue. So, for each of the kinds of
  # expressions that are valid lvalues -- names, subscripts, fields, etc. --
  # we pass in whether or not it appears in a context loose enough to allow
  # "=". If so, it will parse the "=" itself and handle it appropriately.
  let canAssign = precedence <= PREC_CONDITIONAL
  prefix(compiler, canAssign)

  while precedence <= rules[compiler.parser.current.kind].precedence:
    nextToken(compiler.parser[])
    rules[compiler.parser.previous.kind].infix(compiler, canAssign)

# Marks the beginning of a loop. Keeps track of the current instruction so we
# know what to loop back to at the end of the body.
proc startLoop(compiler: Compiler, loop: var Loop) =
  loop.enclosing = compiler.loop
  loop.start = compiler.fn.code.len - 1
  loop.scopeDepth = compiler.scopeDepth
  compiler.loop = addr loop

# Emits the [CODE_JUMP_IF] instruction used to test the loop condition and
# potentially exit the loop. Keeps track of the instruction so we can patch it
# later once we know where the end of the body is.
proc testExitLoop(compiler: Compiler) =
  compiler.loop.exitJump = emitJump(compiler, CODE_JUMP_IF)

# Compiles the body of the loop and tracks its extent so that contained "break"
# statements can be handled correctly.
proc loopBody(compiler: Compiler) =
  compiler.loop.body = compiler.fn.code.len
  statement(compiler)

# Ends the current innermost loop. Patches up all jumps and breaks now that
# we know where the end of the loop is.
proc endLoop(compiler: Compiler) =
  # We don't check for overflow here since the forward jump over the loop body
  # will report an error for the same problem.
  let loopOffset = compiler.fn.code.len - compiler.loop.start + 2
  emitShortArg(compiler, CODE_LOOP, loopOffset)

  patchJump(compiler, compiler.loop.exitJump)

  # Find any break placeholder instructions (which will be CODE_END in the
  # bytecode) and replace them with real jumps.
  var i = compiler.loop.body
  while i < compiler.fn.code.len:
    if compiler.fn.code.data[i].byte == CODE_END.byte:
      compiler.fn.code.data[i] = CODE_JUMP.byte
      patchJump(compiler, i + 1)
      inc i, 3
    else:
      # Skip this instruction and its arguments.
      inc i, 1 + getNumArguments(compiler.fn.code.data,
                               compiler.fn.constants.data, i)

  compiler.loop = compiler.loop.enclosing

proc forStatement(compiler: Compiler) =
  # A for statement like:
  
  #     for (i in sequence.expression) {
  #       System.print(i)
  #     }
  
  # Is compiled to bytecode almost as if the source looked like this:
  
  #     {
  #       var seq_ = sequence.expression
  #       var iter_
  #       while (iter_ = seq_.iterate(iter_)) {
  #         var i = seq_.iteratorValue(iter_)
  #         System.print(i)
  #       }
  #     }
  
  # It's not exactly this, because the synthetic variables `seq_` and `iter_`
  # actually get names that aren't valid Wren identfiers, but that's the basic
  # idea.
  
  # The important parts are:
  # - The sequence expression is only evaluated once.
  # - The .iterate() method is used to advance the iterator and determine if
  #   it should exit the loop.
  # - The .iteratorValue() method is used to get the value at the current
  #   iterator position.

  # Create a scope for the hidden local variables used for the iterator.
  pushScope(compiler)

  consume(compiler, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.")
  consume(compiler, TOKEN_NAME, "Expect for loop variable name.")

  # Remember the name of the loop variable.
  let name = compiler.parser.previous.start
  let length = compiler.parser.previous.length

  consume(compiler, TOKEN_IN, "Expect 'in' after loop variable.")
  ignoreNewlines(compiler)

  # Evaluate the sequence expression and store it in a hidden local variable.
  # The space in the variable name ensures it won't collide with a user-defined
  # variable.
  expression(compiler)
  let seqSlot = addLocal(compiler, "seq ", 4)

  # Create another hidden local for the iterator object.
  null(compiler, false)
  let iterSlot = addLocal(compiler, "iter ", 5)

  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after loop expression.")

  var loop: Loop
  startLoop(compiler, loop)

  # Advance the iterator by calling the ".iterate" method on the sequence.
  loadLocal(compiler, seqSlot)
  loadLocal(compiler, iterSlot)

  # Update and test the iterator.
  callMethod(compiler, 1, "iterate(_)", 10)
  discard emitByteArg(compiler, CODE_STORE_LOCAL, iterSlot)
  testExitLoop(compiler)

  # Get the current value in the sequence by calling ".iteratorValue".
  loadLocal(compiler, seqSlot)
  loadLocal(compiler, iterSlot)
  callMethod(compiler, 1, "iteratorValue(_)", 16)

  # Bind the loop variable in its own scope. This ensures we get a fresh
  # variable each iteration so that closures for it don't all see the same one.
  pushScope(compiler)
  discard addLocal(compiler, name, length)

  loopBody(compiler)

  # Loop variable.
  popScope(compiler)

  endLoop(compiler)

  # Hidden variables.
  popScope(compiler)

proc whileStatement(compiler: Compiler) =
  var loop: Loop
  startLoop(compiler, loop)

  # Compile the condition.
  consume(compiler, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.")
  expression(compiler)
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after while condition.")

  testExitLoop(compiler)
  loopBody(compiler)
  endLoop(compiler)


# Compiles a simple statement. These can only appear at the top-level or
# within curly blocks. Simple statements exclude variable binding statements
# like "var" and "class" which are not allowed directly in places like the
# branches of an "if" statement.
#
# Unlike expressions, statements do not leave a value on the stack.
proc statement(compiler: Compiler) =
  if match(compiler, TOKEN_BREAK):
    if compiler.loop == nil:
      error(compiler, "Cannot use 'break' outside of a loop.")
      return

    # Since we will be jumping out of the scope, make sure any locals in it
    # are discarded first.
    discard discardLocals(compiler, compiler.loop.scopeDepth + 1)

    # Emit a placeholder instruction for the jump to the end of the body. When
    # we're done compiling the loop body and know where the end is, we'll
    # replace these with `CODE_JUMP` instructions with appropriate offsets.
    # We use `CODE_END` here because that can't occur in the middle of
    # bytecode.
    discard emitJump(compiler, CODE_END)
    return

  if match(compiler, TOKEN_FOR):
    forStatement(compiler)
    return

  if match(compiler, TOKEN_IF):
    # Compile the condition.
    consume(compiler, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.")
    expression(compiler)
    consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after if condition.")

    # Jump to the else branch if the condition is false.
    let ifJump = emitJump(compiler, CODE_JUMP_IF)

    # Compile the then branch.
    statement(compiler)

    # Compile the else branch if there is one.
    if match(compiler, TOKEN_ELSE):
      # Jump over the else branch when the if branch is taken.
      let elseJump = emitJump(compiler, CODE_JUMP)
      patchJump(compiler, ifJump)
      
      statement(compiler)

      # Patch the jump over the else.
      patchJump(compiler, elseJump)
    else:
      patchJump(compiler, ifJump);

    return

  if match(compiler, TOKEN_RETURN):
    # Compile the return value.
    if peek(compiler) == TOKEN_LINE:
      # Implicitly return null if there is no value.
      emitOp(compiler, CODE_NULL)
    else:
      expression(compiler)

    emitOp(compiler, CODE_RETURN)
    return

  if (match(compiler, TOKEN_WHILE)):
    whileStatement(compiler)
    return

  # Block statement.
  if (match(compiler, TOKEN_LEFT_BRACE)):
    pushScope(compiler)
    if finishBlock(compiler):
      # Block was an expression, so discard it.
      emitOp(compiler, CODE_POP)
    popScope(compiler)
    return
  
  # Expression statement.
  expression(compiler)
  emitOp(compiler, CODE_POP)

# Creates a matching constructor method for an initializer with [signature]
# and [initializerSymbol].
#
# Construction is a two-stage process in Wren that involves two separate
# methods. There is a static method that allocates a new instance of the class.
# It then invokes an initializer method on the new instance, forwarding all of
# the constructor arguments to it.
#
# The allocator method always has a fixed implementation:
#
#     CODE_CONSTRUCT - Replace the class in slot 0 with a new instance of it.
#     CODE_CALL      - Invoke the initializer on the new instance.
#
# This creates that method and calls the initializer with [initializerSymbol].
proc createConstructor(compiler: Compiler, signature: Signature,
                              initializerSymbol: int) =

  var methodCompiler: WrenCompiler
  init(methodCompiler, compiler.parser, compiler, false)
  
  # Allocate the instance.
  emitOp(addr methodCompiler, 
    if compiler.enclosingClass.isForeign: CODE_FOREIGN_CONSTRUCT 
    else: CODE_CONSTRUCT)
  
  # Run its initializer.
  emitShortArg(addr methodCompiler, (Code)(CODE_CALL_0.int + signature.arity),
               initializerSymbol)
  
  # Return the instance.
  emitOp(addr methodCompiler, CODE_RETURN)
  
  discard endCompiler(addr methodCompiler, "", 0)

# Loads the enclosing class onto the stack and then binds the function already
# on the stack as a method on that class.
proc defineMethod(compiler: Compiler, classVariable: Variable,
                         isStatic: bool, methodSymbol: int) =
  # Load the class. We have to do this for each method because we can't
  # keep the class on top of the stack. If there are static fields, they
  # will be locals above the initial variable slot for the class on the
  # stack. To skip past those, we just load the class each time right before
  # defining a method.
  loadVariable(compiler, classVariable)

  # Define the method.
  let instruction = if isStatic: CODE_METHOD_STATIC else: CODE_METHOD_INSTANCE
  emitShortArg(compiler, instruction, methodSymbol)


# Declares a method in the enclosing class with [signature].
#
# Reports an error if a method with that signature is already declared.
# Returns the symbol for the method.
proc declareMethod(compiler: Compiler, signature: Signature,
                         name: cstring, length: int): int =
  result = signatureSymbol(compiler, signature)
  
  # See if the class has already declared method with this signature.
  let clas = compiler.enclosingClass
  let methods = 
    if clas.inStatic: addr clas.staticMethods else: addr clas.methods
  for i in 0..<methods[].len:
    if methods.data[i] == result:
#      let staticPrefix: cstring = if clas.inStatic: "static " else: ""
      error(compiler, "Class %s already defines a %smethod '%s'.")
#            &compiler->enclosingClass->name->value, staticPrefix, name);
      break
  
  add(compiler.parser.vm, methods[], result)


# Compiles a method definition inside a class body.
#
# Returns `true` if it compiled successfully, or `false` if the method couldn't
# be parsed.
proc meth(compiler: Compiler, classVariable: Variable): bool =
  # TODO: What about foreign constructors?
  let isForeign = match(compiler, TOKEN_FOREIGN)
  let isStatic = match(compiler, TOKEN_STATIC)
  compiler.enclosingClass.inStatic = isStatic
    
  let signatureFn = rules[compiler.parser.current.kind].meth
  nextToken(compiler.parser[])
  
  if signatureFn == nil:
    error(compiler, "Expect method definition.")
    return false
  
  # Build the method signature.
  var signature: Signature 
  signatureFromToken(compiler, signature, skGetter)
  compiler.enclosingClass.signature = addr signature

  var methodCompiler: WrenCompiler
  init(methodCompiler, compiler.parser, compiler, false)

  # Compile the method signature.
  signatureFn(addr methodCompiler, signature)
  
  if isStatic and signature.kind == skInitializer:
    error(compiler, "A constructor cannot be static.")
  
  # Include the full signature in debug messages in stack traces.
  var fullSignature: array[MAX_METHOD_SIGNATURE, char]
  let length = signatureToString(signature, fullSignature)

  # Check for duplicate methods. Doesn't matter that it's already been
  # defined, error will discard bytecode anyway.
  # Check if the method table already contains this symbol
  let methodSymbol = declareMethod(compiler, signature, fullSignature, length)
  
  if isForeign:
    # Define a constant for the signature.
    emitConstant(compiler, newString(compiler.parser.vm,
                                         fullSignature, length).val)

    # We don't need the function we started compiling in the parameter list
    # any more.
    methodCompiler.parser.vm.compiler = methodCompiler.parent
  else:
    consume(compiler, TOKEN_LEFT_BRACE, "Expect '{' to begin method body.")
    finishBody(addr methodCompiler, signature.kind == skInitializer)
    discard endCompiler(addr methodCompiler, fullSignature, length)
  
  # Define the method. For a constructor, this defines the instance
  # initializer method.
  defineMethod(compiler, classVariable, isStatic, methodSymbol)

  if signature.kind == skInitializer:
    # Also define a matching constructor method on the metaclass.
    signature.kind = skMethod
    let constructorSymbol = signatureSymbol(compiler, signature)
    
    createConstructor(compiler, signature, methodSymbol)
    defineMethod(compiler, classVariable, true, constructorSymbol)

  result = true

# Compiles a class definition. Assumes the "class" token has already been
# consumed (along with a possibly preceding "foreign" token).
proc classDefinition(compiler: Compiler, isForeign: bool) =
  # Create a variable to store the class in.
  var classVariable: Variable
  classVariable.scope = if compiler.scopeDepth == -1: SCOPE_MODULE else: SCOPE_LOCAL
  classVariable.index = declareNamedVariable(compiler)
  
  # Create shared class name value
  let className = newString(compiler.parser.vm, 
    compiler.parser.previous.start, compiler.parser.previous.length)
    
  # Make a string constant for the name.
  emitConstant(compiler, className.val)

  # Load the superclass (if there is one).
  if match(compiler, TOKEN_IS):
    parsePrecedence(compiler, PREC_CALL)
  else:
    # Implicitly inherit from Object.
    loadCoreVariable(compiler, "Object")

  # Store a placeholder for the number of fields argument. We don't know
  # the value until we've compiled all the methods to see which fields are
  # used.
  var numFieldsInstruction = -1
  if isForeign:
    emitOp(compiler, CODE_FOREIGN_CLASS)
  else:
    numFieldsInstruction = emitByteArg(compiler, CODE_CLASS, 255)

  # Store it in its name.
  defineVariable(compiler, classVariable.index)

  # Push a local variable scope. Static fields in a class body are hoisted out
  # into local variables declared in this scope. Methods that use them will
  # have upvalues referencing them.
  pushScope(compiler)

  var classCompiler: ClassCompiler
  classCompiler.isForeign = isForeign
  classCompiler.name = className

  # Set up a symbol table for the class's fields. We'll initially compile
  # them to slots starting at zero. When the method is bound to the class, the
  # bytecode will be adjusted by [wrenBindMethod] to take inherited fields
  # into account.
  # init(classCompiler.fields)
  
  # Set up symbol buffers to track duplicate static and instance methods.
  # wrenIntBufferInit(&classCompiler.methods);
  # wrenIntBufferInit(&classCompiler.staticMethods);
  compiler.enclosingClass = addr classCompiler

  # Compile the method definitions.
  consume(compiler, TOKEN_LEFT_BRACE, "Expect '{' after class declaration.")
  discard matchLine(compiler)

  while not match(compiler, TOKEN_RIGHT_BRACE):
    if not meth(compiler, classVariable):
      break
    
    # Don't require a newline after the last definition.
    if match(compiler, TOKEN_RIGHT_BRACE):
      break

    consumeLine(compiler, "Expect newline after definition in class.")
  
  # Update the class with the number of fields.
  if not isForeign:
    compiler.fn.code.data[numFieldsInstruction] = (byte)classCompiler.fields.len
  
  # Clear symbol tables for tracking field and method names.
  clear(compiler.parser.vm, classCompiler.fields)
  clear(compiler.parser.vm, classCompiler.methods)
  clear(compiler.parser.vm, classCompiler.staticMethods)
  compiler.enclosingClass = nil
  popScope(compiler)


# Compiles an "import" statement.
#
# An import just desugars to calling a few special core methods. Given:
#
#     import "foo" for Bar, Baz
#
# We compile it to:
#
#     System.importModule("foo")
#     var Bar = System.getModuleVariable("foo", "Bar")
#     var Baz = System.getModuleVariable("foo", "Baz")
proc importStatement(compiler: Compiler) =
  ignoreNewlines(compiler)
  consume(compiler, TOKEN_STRING, "Expect a string after 'import'.")
  let moduleConstant = addConstant(compiler, compiler.parser.previous.value)

  # Load the module.
  loadCoreVariable(compiler, "System")
  emitShortArg(compiler, CODE_CONSTANT, moduleConstant)
  callMethod(compiler, 1, "importModule(_)", 15)

  # Discard the unused result value from calling the module's fiber.
  emitOp(compiler, CODE_POP)

  # The for clause is optional.
  if not match(compiler, TOKEN_FOR):
    return

  # Compile the comma-separated list of variables to import.
  while true:
    ignoreNewlines(compiler)
    let slot = declareNamedVariable(compiler)

    # Define a string constant for the variable name.
    let variableConstant = addConstant(compiler,
        newString(compiler.parser.vm,
                      compiler.parser.previous.start,
                      compiler.parser.previous.length).val)

    # Load the variable from the other module.
    loadCoreVariable(compiler, "System")
    emitShortArg(compiler, CODE_CONSTANT, moduleConstant)
    emitShortArg(compiler, CODE_CONSTANT, variableConstant)
    callMethod(compiler, 2, "getModuleVariable(_,_)", 22)
    
    # Store the result in the variable here.
    defineVariable(compiler, slot)
    if not match(compiler, TOKEN_COMMA):
      break

# Compiles a "var" variable definition statement.
proc variableDefinition(compiler: Compiler) =
  # Grab its name, but don't declare it yet. A (local) variable shouldn't be
  # in scope in its own initializer.
  consume(compiler, TOKEN_NAME, "Expect variable name.")
  var nameToken = compiler.parser.previous

  # Compile the initializer.
  if match(compiler, TOKEN_EQ):
    ignoreNewlines(compiler)
    expression(compiler)
  else:
    # Default initialize it to null.
    null(compiler, false)

  # Now put it in scope.
  let symbol = declareVariable(compiler, addr nameToken)
  defineVariable(compiler, symbol);


# Compiles a "definition". These are the statements that bind new variables.
# They can only appear at the top level of a block and are prohibited in places
# like the non-curly body of an if or while.
proc definition(compiler: Compiler) =
  if match(compiler, TOKEN_CLASS):
    classDefinition(compiler, false)
  elif match(compiler, TOKEN_FOREIGN):
    consume(compiler, TOKEN_CLASS, "Expect 'class' after 'foreign'.")
    classDefinition(compiler, true)
  elif match(compiler, TOKEN_IMPORT):
    importStatement(compiler)
  elif (match(compiler, TOKEN_VAR)):
    variableDefinition(compiler)
  else:
    statement(compiler)

proc compile*(vm: VM, module: ObjModule, source: cstring, 
    isExpression, printErrors: bool): ObjFn =
  var parser: Parser
  parser.vm = vm
  parser.module = module
  parser.source = source

  parser.tokenStart = source
  parser.currentChar = source
  parser.currentLine = 1
  parser.numParens = 0

  # Zero-init the current token. This will get copied to previous when
  # advance() is called below.
  parser.current.kind = TOKEN_ERROR
  parser.current.start = source
  parser.current.length = 0
  parser.current.line = 0
  parser.current.value = UndefinedVal

  # Ignore leading newlines.
  parser.skipNewlines = true
  parser.printErrors = printErrors
  parser.hasError = false

  # Read the first token.
  nextToken(parser)

  let numExistingVariables = module.variables.len
  var wrenc: WrenCompiler
  let compiler = addr wrenc

  init(wrenc, addr parser, nil, true)
  ignoreNewlines(compiler)

  if isExpression:
    expression(compiler)
  else:
    while not match(compiler, TOKEN_EOF):
      definition(compiler)
      # If there is no newline, it must be the end of the block on the same line.
      if not matchLine(compiler):
        consume(compiler, TOKEN_EOF, "Expect end of file.")
        break
    
    emitOp(compiler, CODE_NULL)
  emitOp(compiler, CODE_RETURN)

  # See if there are any implicitly declared module-level variables that never
  # got an explicit definition. They will have values that are numbers
  # indicating the line where the variable was first used.
  for i in numExistingVariables..<parser.module.variables.len:
    if isNum(parser.module.variables.data[i]):
      # Synthesize a token for the original use site.
      parser.previous.kind = TOKEN_NAME
      parser.previous.start = parser.module.variableNames.data[i].buffer
      parser.previous.length = parser.module.variableNames.data[i].length
      parser.previous.line = (int)asNum(parser.module.variables.data[i])
      error(compiler, "Variable is used but not defined.")
  
  result = endCompiler(compiler, "(script)", 8)

# Parses an expression. Unlike statements, expressions leave a resulting value
# on the stack.
proc expression(compiler: Compiler) =
  parsePrecedence(compiler, PREC_LOWEST)

