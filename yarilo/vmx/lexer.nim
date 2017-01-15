
include compiler 

type
  Keyword = object
    ident: cstring
    length: int
    kind: TokenKind

const
  keywords = [
    Keyword(ident: "break", length: 5, kind: TOKEN_BREAK)
  ]

var errno {.importc, header: "<errno.h>".}: cint

proc c_strtod(buf: cstring, endptr: ptr cstring): float64 {.
  importc: "strtod", header: "<stdlib.h>", noSideEffect.}

proc isName(c: char): bool =
  (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_'

proc isDigit(c: char): bool =
  (c >= '0' and c <= '9')

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

proc makeNumber(parser: var Parser, isHex: bool) =
  errno = 0
  if isHex:
    parser.lexError "hex not supported"
  else:
    parser.current.value = NumVal(c_strtod(parser.tokenStart, nil))

  if errno != 0:
    parser.lexError "parsing error"
    parser.current.value = NumVal(0)

  parser.makeToken TOKEN_NUMBER

proc readHexNumber(parser: var Parser) =
  echo "not implemented"

proc readNumber(parser: var Parser) =
  while parser.peekChar.isDigit:
    discard parser.nextChar
  if parser.peekChar == '.':
    parser.lexError "not implemented"

  if parser.peekChar == 'e' or parser.peekChar == 'E':
    parser.lexError "not implemented"

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
      parser.gc.add(str, c)
      
  parser.current.value = parser.gc.newString(str.data, int(str.count)).asVal
  parser.gc.clear(str)
  parser.makeToken kind

proc twoCharToken(parser: var Parser, c: char, two, one: TokenKind) =
  makeToken(parser, if matchChar(parser, c): two else: one)

proc skipLineComment(parser: var Parser) =
  while peekChar(parser) != LF and peekChar(parser) != char(0):
    discard nextChar(parser)

proc skipBlockComment(parser: var Parser) =
  echo "not implemented"

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
    of ' ', CR, '\t':
      while parser.peekChar == ' ' or parser.peekChar == CR or
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

