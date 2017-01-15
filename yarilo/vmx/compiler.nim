
include opcodes

const
  MaxInterpolationNesting = 8
  LF = char(10)
  CR = char(13)

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
    gc: GC
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
    kind: SignatureKind
    name: cstring
    length: int
    arity: int

  ClassCompiler = object
    name: ObjString
    fields: SymbolTable
    methods: Buffer[int]
    staticMethods: Buffer[int]

    isForeign: bool
    inStatic: bool

    signature: ptr Signature

  Compiler = object
    gc: GC
    parser: ptr Parser
    parent: ptr Compiler
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

proc init(compiler: var Compiler, parser: ptr Parser, parent: ptr Compiler, isFunction: bool) =
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
  compiler.fn = compiler.gc.newFunction(parser.module, compiler.numLocals)

proc error(compiler: var Compiler, format: cstring) =
  echo "error: ", format

