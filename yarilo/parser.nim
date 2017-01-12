
import utils 

const
  MaxInterpolationNesting = 8

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

  Value = object

  Token = object
    kind: TokenKind
    start: cstring
    length: int
    line: int
    value: Value

  Parser = object
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

proc lexError(parser: Parser, error: cstring) =
  echo error

proc inc(p: var cstring) = 
  p = cast[cstring](cast[int](p) +% 1)

proc dec(p: var cstring) = 
  p = cast[cstring](cast[int](p) -% 1)

proc peekChar(parser: Parser): char = parser.currentChar[0]

proc nextChar(parser: var Parser): char = 
  result = parser.peekChar
  inc parser.currentChar
  if result == (char)13:
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

proc readString(parser: var Parser) =
  var 
    str: Buffer[char]
    kind = TOKEN_STRING

  while true:
    let c = parser.nextChar
    case c
    of '"': break
    of char(0):
      parser.lexError("Unterminated string.")
      dec parser.currentChar
      break
    of '%':
      if parser.numParens < MaxInterpolationNesting:
        if parser.nextChar != '(':
          parser.lexError "Expect '(' after '%%'."
        inc parser.numParens
        parser.parens[parser.numParens] = 1
        kind = TOKEN_INTERPOLATION
        break
      parser.lexError "Interpolation may only nest %d levels deep."
    of '\\':
      parser.lexError "Not implemented."
    else:
      parser.add(str, c)
      
  parser.current.value = newString()


proc twoCharToken(parser: var Parser, c: char, two, one: TokenKind) =
  makeToken(parser, if matchChar(parser, c): two else: one)

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
