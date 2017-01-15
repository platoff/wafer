
include scopes

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

  GrammarFn = proc(compiler: var Compiler, canAssign: bool) {.nimcall.}
  SignatureFn = proc(compiler: var Compiler, signature: ptr Signature) {.nimcall.}

  GrammarRule = object
    prefix: GrammarFn
    infix: GrammarFn
    meth: SignatureFn
    precedence: Precedence
    name: cstring

# Forward declarations since the grammar is recursive.
#proc getRule(TokenKind kind): ptr GrammarRule
proc expression(compiler: var Compiler)
#proc statement(compiler: var Compiler)
#proc definition(compiler: var Compiler)
#proc parsePrecedence(compiler: var Compiler, precedence: Precedence)

# Gets the symbol for a method [name] with [length].
proc methodSymbol(compiler: var Compiler, name: cstring, length: int): int =
  compiler.gc.ensure(compiler.parser.vm.methodNames, name, length);
}

# Compiles a method call with [numArgs] for a method with [name] with [length].
proc callMethod(compiler: var Compiler, numArgs: int, name: cstring, length: int) =
  let symbol = methodSymbol(compiler, name, length)
  emitShortArg(compiler, (Code)(CODE_CALL_0.int + numArgs), symbol)

# Pushes the value for a module-level variable implicitly imported from core.
proc loadCoreVariable(compiler: var Compiler, name: cstring) =
  var symbol = find(compiler.parser.module.variableNames, name, name.len)
  assert(symbol != -1, "Should have already defined core name.")
  emitShortArg(compiler, CODE_LOAD_MODULE_VAR, symbol)

# A parenthesized expression.
proc grouping(compiler: var Compiler, canAssign: bool) =
  expression(compiler)
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after expression.")

# A list literal.
proc list(compiler: var Compiler, canAssign: bool) =
  # Instantiate a new list.
  loadCoreVariable(compiler, "List")
  callMethod(compiler, 0, "new()", 5);
  
  // Compile the list elements. Each one compiles to a ".add()" call.
  do
  {
    ignoreNewlines(compiler);

    // Stop if we hit the end of the list.
    if (peek(compiler) == TOKEN_RIGHT_BRACKET) break;

    // The element.
    expression(compiler);
    callMethod(compiler, 1, "addCore_(_)", 11);
  } while (match(compiler, TOKEN_COMMA));

  // Allow newlines before the closing ']'.
  ignoreNewlines(compiler);
  consume(compiler, TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.");
}

// A map literal.
static void map(Compiler* compiler, bool canAssign)
{
  // Instantiate a new map.
  loadCoreVariable(compiler, "Map");
  callMethod(compiler, 0, "new()", 5);

  // Compile the map elements. Each one is compiled to just invoke the
  // subscript setter on the map.
  do
  {
    ignoreNewlines(compiler);

    // Stop if we hit the end of the map.
    if (peek(compiler) == TOKEN_RIGHT_BRACE) break;

    // The key.
    parsePrecedence(compiler, PREC_UNARY);
    consume(compiler, TOKEN_COLON, "Expect ':' after map key.");
    ignoreNewlines(compiler);

    // The value.
    expression(compiler);
    callMethod(compiler, 2, "addCore_(_,_)", 13);
  } while (match(compiler, TOKEN_COMMA));

  // Allow newlines before the closing '}'.
  ignoreNewlines(compiler);
  consume(compiler, TOKEN_RIGHT_BRACE, "Expect '}' after map entries.");
}

# Unary operators like `-foo`.
proc unaryOp(compiler: var Compiler, canAssign: bool) =
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  ignoreNewlines(compiler);

  // Compile the argument.
  parsePrecedence(compiler, (Precedence)(PREC_UNARY + 1));

  // Call the operator method on the left-hand side.
  callMethod(compiler, 0, rule->name, 1);

proc boolean(compiler: var Compiler, canAssign: bool) =
  emitOp(compiler,
      compiler->parser->previous.type == TOKEN_FALSE ? CODE_FALSE : CODE_TRUE);

// Walks the compiler chain to find the compiler for the nearest class
// enclosing this one. Returns NULL if not currently inside a class definition.
static Compiler* getEnclosingClassCompiler(Compiler* compiler)
{
  while (compiler != NULL)
  {
    if (compiler->enclosingClass != NULL) return compiler;
    compiler = compiler->parent;
  }

  return NULL;
}

// Walks the compiler chain to find the nearest class enclosing this one.
// Returns NULL if not currently inside a class definition.
static ClassCompiler* getEnclosingClass(Compiler* compiler)
{
  compiler = getEnclosingClassCompiler(compiler);
  return compiler == NULL ? NULL : compiler->enclosingClass;
}

static void field(Compiler* compiler, bool canAssign)
{
  // Initialize it with a fake value so we can keep parsing and minimize the
  // number of cascaded errors.
  int field = 255;

  ClassCompiler* enclosingClass = getEnclosingClass(compiler);

  if (enclosingClass == NULL)
  {
    error(compiler, "Cannot reference a field outside of a class definition.");
  }
  else if (enclosingClass->isForeign)
  {
    error(compiler, "Cannot define fields in a foreign class.");
  }
  else if (enclosingClass->inStatic)
  {
    error(compiler, "Cannot use an instance field in a static method.");
  }
  else
  {
    // Look up the field, or implicitly define it.
    field = wrenSymbolTableEnsure(compiler->parser->vm, &enclosingClass->fields,
        compiler->parser->previous.start,
        compiler->parser->previous.length);

    if (field >= MAX_FIELDS)
    {
      error(compiler, "A class can only have %d fields.", MAX_FIELDS);
    }
  }

  // If there's an "=" after a field name, it's an assignment.
  bool isLoad = true;
  if (canAssign && match(compiler, TOKEN_EQ))
  {
    // Compile the right-hand side.
    expression(compiler);
    isLoad = false;
  }

  // If we're directly inside a method, use a more optimal instruction.
  if (compiler->parent != NULL &&
      compiler->parent->enclosingClass == enclosingClass)
  {
    emitByteArg(compiler, isLoad ? CODE_LOAD_FIELD_THIS : CODE_STORE_FIELD_THIS,
                field);
  }
  else
  {
    loadThis(compiler);
    emitByteArg(compiler, isLoad ? CODE_LOAD_FIELD : CODE_STORE_FIELD, field);
  }
}

// Compiles a read or assignment to [variable].
static void bareName(Compiler* compiler, bool canAssign, Variable variable)
{
  // If there's an "=" after a bare name, it's a variable assignment.
  if (canAssign && match(compiler, TOKEN_EQ))
  {
    // Compile the right-hand side.
    expression(compiler);

    // Emit the store instruction.
    switch (variable.scope)
    {
      case SCOPE_LOCAL:
        emitByteArg(compiler, CODE_STORE_LOCAL, variable.index);
        break;
      case SCOPE_UPVALUE:
        emitByteArg(compiler, CODE_STORE_UPVALUE, variable.index);
        break;
      case SCOPE_MODULE:
        emitShortArg(compiler, CODE_STORE_MODULE_VAR, variable.index);
        break;
      default:
        UNREACHABLE();
    }
    return;
  }

  // Emit the load instruction.
  loadVariable(compiler, variable);
}

static void staticField(Compiler* compiler, bool canAssign)
{
  Compiler* classCompiler = getEnclosingClassCompiler(compiler);
  if (classCompiler == NULL)
  {
    error(compiler, "Cannot use a static field outside of a class definition.");
    return;
  }

  // Look up the name in the scope chain.
  Token* token = &compiler->parser->previous;

  // If this is the first time we've seen this static field, implicitly
  // define it as a variable in the scope surrounding the class definition.
  if (resolveLocal(classCompiler, token->start, token->length) == -1)
  {
    int symbol = declareVariable(classCompiler, NULL);

    // Implicitly initialize it to null.
    emitOp(classCompiler, CODE_NULL);
    defineVariable(classCompiler, symbol);
  }

  // It definitely exists now, so resolve it properly. This is different from
  // the above resolveLocal() call because we may have already closed over it
  // as an upvalue.
  Variable variable = resolveName(compiler, token->start, token->length);
  bareName(compiler, canAssign, variable);
}

// Returns `true` if [name] is a local variable name (starts with a lowercase
// letter).
static bool isLocalName(const char* name)
{
  return name[0] >= 'a' && name[0] <= 'z';
}

// Compiles a variable name or method call with an implicit receiver.
static void name(Compiler* compiler, bool canAssign)
{
  // Look for the name in the scope chain up to the nearest enclosing method.
  Token* token = &compiler->parser->previous;

  Variable variable = resolveNonmodule(compiler, token->start, token->length);
  if (variable.index != -1)
  {
    bareName(compiler, canAssign, variable);
    return;
  }

  // TODO: The fact that we return above here if the variable is known and parse
  // an optional argument list below if not means that the grammar is not
  // context-free. A line of code in a method like "someName(foo)" is a parse
  // error if "someName" is a defined variable in the surrounding scope and not
  // if it isn't. Fix this. One option is to have "someName(foo)" always
  // resolve to a self-call if there is an argument list, but that makes
  // getters a little confusing.

  // If we're inside a method and the name is lowercase, treat it as a method
  // on this.
  if (isLocalName(token->start) && getEnclosingClass(compiler) != NULL)
  {
    loadThis(compiler);
    namedCall(compiler, canAssign, CODE_CALL_0);
    return;
  }

  // Otherwise, look for a module-level variable with the name.
  variable.scope = SCOPE_MODULE;
  variable.index = wrenSymbolTableFind(&compiler->parser->module->variableNames,
                                       token->start, token->length);
  if (variable.index == -1)
  {
    if (isLocalName(token->start))
    {
      error(compiler, "Undefined variable.");
      return;
    }

    // If it's a nonlocal name, implicitly define a module-level variable in
    // the hopes that we get a real definition later.
    variable.index = wrenDeclareVariable(compiler->parser->vm,
                                         compiler->parser->module,
                                         token->start, token->length,
                                         token->line);

    if (variable.index == -2)
    {
      error(compiler, "Too many module variables defined.");
    }
  }
  
  bareName(compiler, canAssign, variable);
}

static void null(Compiler* compiler, bool canAssign)
{
  emitOp(compiler, CODE_NULL);
}

// A number or string literal.
static void literal(Compiler* compiler, bool canAssign)
{
  emitConstant(compiler, compiler->parser->previous.value);
}

// A string literal that contains interpolated expressions.
//
// Interpolation is syntactic sugar for calling ".join()" on a list. So the
// string:
//
//     "a %(b + c) d"
//
// is compiled roughly like:
//
//     ["a ", b + c, " d"].join()
static void stringInterpolation(Compiler* compiler, bool canAssign)
{
  // Instantiate a new list.
  loadCoreVariable(compiler, "List");
  callMethod(compiler, 0, "new()", 5);
  
  do
  {
    // The opening string part.
    literal(compiler, false);
    callMethod(compiler, 1, "addCore_(_)", 11);
    
    // The interpolated expression.
    ignoreNewlines(compiler);
    expression(compiler);
    callMethod(compiler, 1, "addCore_(_)", 11);
    
    ignoreNewlines(compiler);
  } while (match(compiler, TOKEN_INTERPOLATION));
  
  // The trailing string part.
  consume(compiler, TOKEN_STRING, "Expect end of string interpolation.");
  literal(compiler, false);
  callMethod(compiler, 1, "addCore_(_)", 11);
  
  // The list of interpolated parts.
  callMethod(compiler, 0, "join()", 6);
}

static void super_(Compiler* compiler, bool canAssign)
{
  ClassCompiler* enclosingClass = getEnclosingClass(compiler);

  if (enclosingClass == NULL)
  {
    error(compiler, "Cannot use 'super' outside of a method.");
  }

  loadThis(compiler);

  // TODO: Super operator calls.
  // TODO: There's no syntax for invoking a superclass constructor with a
  // different name from the enclosing one. Figure that out.

  // See if it's a named super call, or an unnamed one.
  if (match(compiler, TOKEN_DOT))
  {
    // Compile the superclass call.
    consume(compiler, TOKEN_NAME, "Expect method name after 'super.'.");
    namedCall(compiler, canAssign, CODE_SUPER_0);
  }
  else if (enclosingClass != NULL)
  {
    // No explicit name, so use the name of the enclosing method. Make sure we
    // check that enclosingClass isn't NULL first. We've already reported the
    // error, but we don't want to crash here.
    methodCall(compiler, CODE_SUPER_0, enclosingClass->signature);
  }
}

static void this_(Compiler* compiler, bool canAssign)
{
  if (getEnclosingClass(compiler) == NULL)
  {
    error(compiler, "Cannot use 'this' outside of a method.");
    return;
  }

  loadThis(compiler);
}

// Subscript or "array indexing" operator like `foo[bar]`.
static void subscript(Compiler* compiler, bool canAssign)
{
  Signature signature = { "", 0, SIG_SUBSCRIPT, 0 };

  // Parse the argument list.
  finishArgumentList(compiler, &signature);
  consume(compiler, TOKEN_RIGHT_BRACKET, "Expect ']' after arguments.");

  if (canAssign && match(compiler, TOKEN_EQ))
  {
    signature.type = SIG_SUBSCRIPT_SETTER;

    // Compile the assigned value.
    validateNumParameters(compiler, ++signature.arity);
    expression(compiler);
  }

  callSignature(compiler, CODE_CALL_0, &signature);
}

static void call(Compiler* compiler, bool canAssign)
{
  ignoreNewlines(compiler);
  consume(compiler, TOKEN_NAME, "Expect method name after '.'.");
  namedCall(compiler, canAssign, CODE_CALL_0);
}

static void and_(Compiler* compiler, bool canAssign)
{
  ignoreNewlines(compiler);

  // Skip the right argument if the left is false.
  int jump = emitJump(compiler, CODE_AND);
  parsePrecedence(compiler, PREC_LOGICAL_AND);
  patchJump(compiler, jump);
}

static void or_(Compiler* compiler, bool canAssign)
{
  ignoreNewlines(compiler);

  // Skip the right argument if the left is true.
  int jump = emitJump(compiler, CODE_OR);
  parsePrecedence(compiler, PREC_LOGICAL_OR);
  patchJump(compiler, jump);
}

static void conditional(Compiler* compiler, bool canAssign)
{
  // Ignore newline after '?'.
  ignoreNewlines(compiler);

  // Jump to the else branch if the condition is false.
  int ifJump = emitJump(compiler, CODE_JUMP_IF);

  // Compile the then branch.
  parsePrecedence(compiler, PREC_CONDITIONAL);

  consume(compiler, TOKEN_COLON,
          "Expect ':' after then branch of conditional operator.");
  ignoreNewlines(compiler);

  // Jump over the else branch when the if branch is taken.
  int elseJump = emitJump(compiler, CODE_JUMP);

  // Compile the else branch.
  patchJump(compiler, ifJump);

  parsePrecedence(compiler, PREC_ASSIGNMENT);

  // Patch the jump over the else.
  patchJump(compiler, elseJump);
}

void infixOp(Compiler* compiler, bool canAssign)
{
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  // An infix operator cannot end an expression.
  ignoreNewlines(compiler);

  // Compile the right-hand side.
  parsePrecedence(compiler, (Precedence)(rule->precedence + 1));

  // Call the operator method on the left-hand side.
  Signature signature = { rule->name, (int)strlen(rule->name), SIG_METHOD, 1 };
  callSignature(compiler, CODE_CALL_0, &signature);
}

// Compiles a method signature for an infix operator.
void infixSignature(Compiler* compiler, Signature* signature)
{
  // Add the RHS parameter.
  signature->type = SIG_METHOD;
  signature->arity = 1;

  // Parse the parameter name.
  consume(compiler, TOKEN_LEFT_PAREN, "Expect '(' after operator name.");
  declareNamedVariable(compiler);
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
}

// Compiles a method signature for an unary operator (i.e. "!").
void unarySignature(Compiler* compiler, Signature* signature)
{
  // Do nothing. The name is already complete.
  signature->type = SIG_GETTER;
}

// Compiles a method signature for an operator that can either be unary or
// infix (i.e. "-").
void mixedSignature(Compiler* compiler, Signature* signature)
{
  signature->type = SIG_GETTER;

  // If there is a parameter, it's an infix operator, otherwise it's unary.
  if (match(compiler, TOKEN_LEFT_PAREN))
  {
    // Add the RHS parameter.
    signature->type = SIG_METHOD;
    signature->arity = 1;

    // Parse the parameter name.
    declareNamedVariable(compiler);
    consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
  }
}

// Compiles an optional setter parameter in a method [signature].
//
// Returns `true` if it was a setter.
static bool maybeSetter(Compiler* compiler, Signature* signature)
{
  // See if it's a setter.
  if (!match(compiler, TOKEN_EQ)) return false;

  // It's a setter.
  if (signature->type == SIG_SUBSCRIPT)
  {
    signature->type = SIG_SUBSCRIPT_SETTER;
  }
  else
  {
    signature->type = SIG_SETTER;
  }

  // Parse the value parameter.
  consume(compiler, TOKEN_LEFT_PAREN, "Expect '(' after '='.");
  declareNamedVariable(compiler);
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");

  signature->arity++;

  return true;
}

// Compiles a method signature for a subscript operator.
void subscriptSignature(Compiler* compiler, Signature* signature)
{
  signature->type = SIG_SUBSCRIPT;

  // The signature currently has "[" as its name since that was the token that
  // matched it. Clear that out.
  signature->length = 0;

  // Parse the parameters inside the subscript.
  finishParameterList(compiler, signature);
  consume(compiler, TOKEN_RIGHT_BRACKET, "Expect ']' after parameters.");

  maybeSetter(compiler, signature);
}

// Parses an optional parenthesized parameter list. Updates `type` and `arity`
// in [signature] to match what was parsed.
static void parameterList(Compiler* compiler, Signature* signature)
{
  // The parameter list is optional.
  if (!match(compiler, TOKEN_LEFT_PAREN)) return;
  
  signature->type = SIG_METHOD;
  
  // Allow an empty parameter list.
  if (match(compiler, TOKEN_RIGHT_PAREN)) return;

  finishParameterList(compiler, signature);
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

// Compiles a method signature for a named method or setter.
void namedSignature(Compiler* compiler, Signature* signature)
{
  signature->type = SIG_GETTER;
  
  // If it's a setter, it can't also have a parameter list.
  if (maybeSetter(compiler, signature)) return;

  // Regular named method with an optional parameter list.
  parameterList(compiler, signature);
}

// Compiles a method signature for a constructor.
void constructorSignature(Compiler* compiler, Signature* signature)
{
  consume(compiler, TOKEN_NAME, "Expect constructor name after 'construct'.");
  
  // Capture the name.
  *signature = signatureFromToken(compiler, SIG_INITIALIZER);
  
  if (match(compiler, TOKEN_EQ))
  {
    error(compiler, "A constructor cannot be a setter.");
  }

  if (!match(compiler, TOKEN_LEFT_PAREN))
  {
    error(compiler, "A constructor cannot be a getter.");
    return;
  }
  
  // Allow an empty parameter list.
  if (match(compiler, TOKEN_RIGHT_PAREN)) return;
  
  finishParameterList(compiler, signature);
  consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}


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
  rules = [
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
    INFIX(PREC_LOGICAL_OR, or_), # /* TOKEN_PIPEPIPE      */ 
    INFIX_OPERATOR(PREC_BITWISE_XOR, "^"), # /* TOKEN_CARET         */ 
    INFIX_OPERATOR(PREC_BITWISE_AND, "&"), # /* TOKEN_AMP           */ 
    INFIX(PREC_LOGICAL_AND, and_), # /* TOKEN_AMPAMP        */ 
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
    { NULL, NULL, constructorSignature, PREC_NONE, NULL }, #     /* TOKEN_CONSTRUCT     */ 
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
    PREFIX(super_), #     /* TOKEN_SUPER         */ 
    PREFIX(this_), #     /* TOKEN_THIS          */ 
    PREFIX(boolean), #     /* TOKEN_TRUE          */ 
    UNUSED, #     /* TOKEN_VAR           */ 
    UNUSED, #     /* TOKEN_WHILE         */ 
    PREFIX(field), #     /* TOKEN_FIELD         */ 
    PREFIX(staticField), #     /* TOKEN_STATIC_FIELD  */ 
    { name, NULL, namedSignature, PREC_NONE, NULL }, #     /* TOKEN_NAME          */ 
    PREFIX(literal), #     /* TOKEN_NUMBER        */ 
    PREFIX(literal), #     /* TOKEN_STRING        */ 
    PREFIX(stringInterpolation), #     /* TOKEN_INTERPOLATION */ 
    UNUSED, #     /* TOKEN_LINE          */ 
    UNUSED, #     /* TOKEN_ERROR         */ 
    UNUSED #     /* TOKEN_EOF           */
  ] 

# The main entrypoint for the top-down operator precedence parser.
proc parsePrecedence(compiler: var Compiler, precedence: Precedence) =
  nextToken(compiler.parser[])
  GrammarFn prefix = rules[compiler->parser->previous.type].prefix;

  if (prefix == NULL)
  {
    error(compiler, "Expected expression.");
    return;
  }

  // Track if the precendence of the surrounding expression is low enough to
  // allow an assignment inside this one. We can't compile an assignment like
  // a normal expression because it requires us to handle the LHS specially --
  // it needs to be an lvalue, not an rvalue. So, for each of the kinds of
  // expressions that are valid lvalues -- names, subscripts, fields, etc. --
  // we pass in whether or not it appears in a context loose enough to allow
  // "=". If so, it will parse the "=" itself and handle it appropriately.
  bool canAssign = precedence <= PREC_CONDITIONAL;
  prefix(compiler, canAssign);

  while (precedence <= rules[compiler->parser->current.type].precedence)
  {
    nextToken(compiler->parser);
    GrammarFn infix = rules[compiler->parser->previous.type].infix;
    infix(compiler, canAssign);
  }
}

proc compile(gc: GC, module: ObjModule, source: cstring, 
    isExpression, printErrors: bool): ObjFn =
  var parser: Parser
  parser.gc = gc
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

  let numExistingVariables = int(module.variables.count)

  var compiler: Compiler
  init(compiler, addr parser, nil, true)
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
  for i in numExistingVariables..<parser.module.variables.count:
    if isNum(parser.module.variables.data[i]):
      # Synthesize a token for the original use site.
      parser.previous.kind = TOKEN_NAME
      parser.previous.start = parser.module.variableNames.data[i].buffer
      parser.previous.length = parser.module.variableNames.data[i].length
      parser.previous.line = (int)asNum(parser.module.variables.data[i])
      error(compiler, "Variable is used but not defined.")
  
  result = endCompiler(compiler, "(script)", 8)

// Parses an expression. Unlike statements, expressions leave a resulting value
// on the stack.
void expression(Compiler* compiler)
{
  parsePrecedence(compiler, PREC_LOWEST);
}
