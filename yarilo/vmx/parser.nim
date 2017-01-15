
include lexer

# Parsing ---------------------------------

# Returns the type of the current token.
proc peek(compiler: var Compiler): TokenKind =
  compiler.parser.current.kind

# Consumes the current token if its type is [expected]. Returns true if a
# token was consumed.
proc match(compiler: var Compiler, expected: TokenKind): bool =
  if peek(compiler) != expected:
    result = false
  else:
    nextToken(compiler.parser[])
    result = true

# Consumes the current token. Emits an error if its type is not [expected].
proc consume(compiler: var Compiler, expected: TokenKind, 
    errorMessage: cstring) =
  nextToken(compiler.parser[])
  if compiler.parser.previous.kind != expected:
    error(compiler, errorMessage)

    # If the next token is the one we want, assume the current one is just a
    # spurious error and discard it to minimize the number of cascaded errors.
    if compiler.parser.current.kind == expected:
      nextToken(compiler.parser[])

# Matches one or more newlines. Returns true if at least one was found.
proc matchLine(compiler: var Compiler): bool =
  if not match(compiler, TOKEN_LINE):
    return false

  while match(compiler, TOKEN_LINE):
    discard
  result = true


# Discards any newlines starting at the current token.
proc ignoreNewlines(compiler: var Compiler) =
  discard matchLine(compiler)


