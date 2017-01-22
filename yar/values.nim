
import hashes
import strutils

type
  ObjectKind = enum
    oString
    oWord
    oBlock
    oFunc
    oFuncNative
    oFuncContext
    oContext
    oVector
    oReactiveContext

  FlexibleArray{.unchecked.}[T] = array[0..0, T]
  PArray[T] = ptr FlexibleArray[T]

  Buffer[T] = object
    data: PArray[T]
    count: int
    capacity: int

  ValueTag = enum
    tagInt
    tagObj
    tagSpecial
    tagConst

  Value = distinct int

  Obj = ptr Object
  Object = object
    codeword: ObjectKind

  ConstArray[T] = ptr ConstArrayObj[T]
  ConstArrayObj[T] = object
    obj: Object
    len: int
    hashCode: Hash
    data: FlexibleArray[T]

  Symbol = Obj # pointer to const

  StringConst = ConstArray[char]
  Block = ConstArray[Value]

  WordKind = enum
    wkWord
    wkSetWord

  Word = ptr WordObj
  WordObj = object
    obj: Object
    symbol: StringConst
    kind: WordKind

  Function = ptr FuncObj
  FuncBase = ptr FuncBaseObj
  FuncBaseObj = object
    obj: Object
    params: int

  FuncObj = object
    base: FuncBaseObj
    locals: FuncContext
    body: Block

  Native = proc(vm: VM) {.nimcall.}

  FuncNative = ptr FuncNativeObj
  FuncNativeObj = object
    base: FuncBaseObj
    f: Native

  Pair = object
    symbol: StringConst
    value: Value

  BaseContext = ptr BaseContextObj
  BaseContextObj = object
    obj: Object
    parent: BaseContext

  Context = ptr ContextObj
  ContextObj = object
    base: BaseContextObj
    pairs: Buffer[Pair]

  FuncContext = ptr FuncContextObj
  FuncContextObj = object
    base: BaseContextObj
    data: Buffer[Symbol]

  Closure = object
    symbol: StringConst
    value: ptr Value

  ReactiveContext = ptr ReactiveContextObj
  ReactiveContextObj = object
    base: BaseContextObj
    closures: Buffer[Closure]

  Vector = ptr VectorObj
  VectorObj = object
    obj: Object
    data: Buffer[Value]

  VM = ptr VMObj
  VMObj = object
    bytesAllocated: int
    bp: int
    stack: Buffer[Value]
    #rstack: Buffer[ptr Value]
    #frames: Buffer[int]
    context: BaseContext
    constants: Buffer[Symbol]

const
  Null = Value((3 shl 2) or tagSpecial.int)
  False = Value((0 shl 2) or tagSpecial.int)
  True = Value((1 shl 2) or tagSpecial.int)

#
# Allocator
#

proc reallocate(vm: VM, memory: pointer; oldSize, newSize: int): pointer =
  inc vm.bytesAllocated, newSize - oldSize
  realloc(memory, newSize)

proc deallocate(vm: VM, p: pointer, oldSize = 0.Natural) {.inline.} =
  discard vm.reallocate( p, oldSize, 0)

proc allocate(vm: VM, size: int): pointer =
  result = vm.reallocate(nil, 0, size)

proc allocate(vm: VM, T: typedesc): ptr T =
  cast[ptr T](vm.reallocate(nil, 0, sizeof T))

#
# Buffers
#

proc init(buf: var Buffer) =
  buf.data = nil
  buf.count = 0
  buf.capacity = 0

#proc len[T](buf: Buffer[T]): int = buf.count

proc clear[T](vm: VM, buf: var Buffer[T]) =
  vm.deallocate buf.data, buf.count * sizeof(T)
  buf.init

proc `[]`[T](buf: Buffer[T], i: int): T =
  assert i < int(buf.count)
  buf.data[i]
  
proc `[]=`*[T](buf: var Buffer[T], i: int, val: T) {.inline.} = 
  assert i < int(buf.count)
  buf.data[i] = val

proc grow[T](vm: VM, buffer: var Buffer[T], len: int) =
  var capacity = buffer.capacity
  if capacity == 0:
    capacity = max(5, len)
  else:
    capacity = max(capacity * 8 div 5, len)
  buffer.data = 
    cast[PArray[T]](
      vm.reallocate(buffer.data, buffer.capacity * sizeof(T), capacity * sizeof(T)))
  buffer.capacity = capacity

proc setLen[T](vm: VM, buffer: var Buffer[T], len: int) =
  if buffer.capacity < len:
    vm.grow buffer, len
  buffer.count = len

proc add[T](vm: VM, buffer: var Buffer[T], val: T) = 
  let last = buffer.count
  vm.setLen(buffer, last + 1)
  buffer.data[last] = val

proc add[T](vm: VM, buffer: var Buffer[T], blk: ptr T, len: int) = 
  let last = buffer.count
  vm.setLen(buffer, last + len)
  copyMem(addr buffer.data[last], blk, len * sizeof T)

proc peek[T](buf: Buffer[T]): T =
  assert buf.count > 0
  buf.data[buf.count - 1]

proc drop(buf: var Buffer)=
  assert buf.count > 0
  dec buf.count

iterator items[T](buf: Buffer[T]): T = 
  for i in 0..< buf.count:
    yield buf.data[i]

#
# Values
#

proc tag(v: Value): ValueTag = ValueTag(int(v) and 0x3)
proc value(b: bool): Value = Value((b.int shl 2) or tagSpecial.int)
proc value(i: int): Value = Value(i shl 2)
proc constValue(p: pointer): Value = Value(cast[int](p) or int(tagConst))

proc hashArray[T](a: ConstArray[T]): Hash =
  result = a.obj.hash
  for i in 0..<a.len:
    result = result !& a.data[i].hash
  result = !$result

proc isNum(v: Value): bool = tag(v) == tagInt

proc asNum(v: Value): int = 
  assert isNum(v)
  int(v) shr 2

proc asPointer(v: Value): Obj =
  assert((int(v) and 1) == 1)
  cast[Obj](int(v) and (not 3))

#
# Objects
#

type
  ObjectType = ConstArray | Word

proc hash(obj: Object): Hash = obj.codeword.hash

proc newConstArray[T](vm: VM, cw: ObjectKind, data: PArray[T], size: int, trailingZero = false): ConstArray[T] =
  let allocate = sizeof(T) * (if trailingZero: size + 1 else: size)
  result = 
    cast[ConstArray[T]](vm.allocate(sizeof(ConstArrayObj[T]) + allocate))
  result.obj.codeword = cw
  result.len = size
  copyMem(addr result.data[0], data, sizeof(T) * size)
  if trailingZero:
    result.data[size] = T(0)
  result.hashCode = hashArray(result)

proc equals[T](a, b: ConstArray[T]): bool =
  a.obj == b.obj and a.len == b.len and a.hashCode == b.hashCode and 
    equalMem(addr a.data[0], addr b.data[0], a.len * sizeof(T))

proc `$`(v: Value): string

converter objDowncast[T: ObjectType](obj: T): Obj = cast[Obj](obj)

#
# String Const
#

proc hash(a: StringConst): Hash = a.hashCode
proc value(s: StringConst): Value = constValue(s)
proc `$`(s: StringConst): string =
  $cast[cstring](addr s.data[0])

#
# Word
#

proc hash(w: Word): Hash = !$(w.obj.hash !& w.symbol.hash !& w.kind.hash)
proc value(s: Word): Value = constValue(s)
proc equals(a, b: Word): bool =
  a.obj == b.obj and a.symbol == b.symbol and a.kind == b.kind
proc `$`(s: Word): string =
  "word: " & $s.symbol

#
# Block
#

proc hash(a: Block): Hash = a.hashCode
proc value(s: Block): Value = constValue(s)
proc `$`(b: Block): string =
  result = "["
  var comma = false
  for i in 0..<b.len:
    if comma: result.add ", "
    else: comma = true
    result.add $(b.data[i])
  result.add ']'

iterator items(blk: Block): Value =
  for i in 0..<blk.len:
    yield blk.data[i]

#
# Function
#

proc newFunc(vm: VM, params: int, locals: FuncContext, body: Block): Function =
  result = vm.allocate(FuncObj)
  result.base.obj.codeword = oFunc
  result.base.params = params
  result.locals = locals
  result.body = body

proc newFunc(vm: VM, params: int, f: Native): FuncNative =
  result = vm.allocate(FuncNativeObj)
  result.base.obj.codeword = oFuncNative
  result.base.params = params
  result.f = f

proc value(f: Function): Value = constValue(f)
proc value(f: FuncNative): Value = constValue(f)

proc `$`(f: Function): string = "func ... " & $f.body
proc `$`(f: FuncNative): string = "native (" & $f.base.params & ")"

#
# Contexts
#

proc init(ctx: var BaseContextObj, kind: ObjectKind, parent: BaseContext) =
  ctx.obj.codeword = kind
  ctx.parent = parent

proc newFuncContext(vm: VM, parent: BaseContext): FuncContext =
  result = vm.allocate FuncContextObj
  init(result.base, oFuncContext, parent)
  init(result.data)

#
# Vector
#

proc newVector(vm: VM): Vector =
  result = vm.allocate(VectorObj)
  result.obj.codeword = oVector
  init(result.data)

proc `$`(v: Vector): string =
  result = "["
  var comma = false
  for i in 0..<v.data.count:
    if comma: result.add ", "
    else: comma = true
    result.add $(v.data[i])
  result.add ']'

proc value(f: Vector): Value = constValue(f)

#
# Values
# 

proc `$`(v: Value): string =
  case v.tag:
  of tagInt: $(v.asNum)
  of tagObj, tagConst: 
    let obj = v.asPointer
    case obj.codeword:
    of oString: $(cast[StringConst](obj))
    of oWord: $(cast[Word](obj))
    of oBlock: $(cast[Block](obj))
    of oFunc: $(cast[Function](obj))
    of oFuncNative: $(cast[FuncNative](obj))
    of oVector: $(cast[Vector](obj))
    else: "[" & $obj.codeword & "]"
  of tagSpecial: 
    case (v.int) shr 2:
    of 0: "false"
    of 1: "true"
    of 2: "null"  
    else: 
      assert false, "unreachable"
      "error"

#
# Constants
#

proc lookup[T](vm: VM, symbol: T): T =
  result = nil
  for i in vm.constants:
    if equals(symbol, cast[T](i)):
      result = cast[T](i)
      break

  if result == nil:
    result = symbol
    vm.add vm.constants, result
  else:
    assert result.pointer != symbol.pointer
    vm.deallocate(symbol)

proc lookup[T](vm: VM, obj: var T): ptr T =
  result = nil
  let symbol = addr obj
  for i in vm.constants:
    if equals(symbol, cast[ptr T](i)):
      result = cast[ptr T](i)
      break

  if result == nil:
    result = cast[ptr T](vm.allocate(sizeof(T)))
    copyMem(result, symbol, sizeof(T))
    vm.add vm.constants, result
  else:
    assert result.pointer != symbol.pointer

proc newString(vm: VM, s: cstring): StringConst =
  vm.lookup vm.newConstArray(oString, cast[PArray[char]](s), s.len, true)

proc newWord(vm: VM, symbol: StringConst, kind: WordKind): Word =
  var w: WordObj
  w.obj.codeword = oWord
  w.symbol = symbol
  w.kind = kind
  result = vm.lookup w

proc newBlock(vm: VM, buf: Buffer[Value]): Block =
  vm.lookup vm.newConstArray(oBlock, buf.data, buf.count)

#
# Context
#

proc newContext(vm: VM, parent: BaseContext): Context =
  result = vm.allocate(ContextObj)
  init(result.base, oContext, parent)
  init(result.pairs)

proc add(vm: VM, ctx: Context, symbol: StringConst, val: Value) =
  let last = ctx.pairs.count
  vm.setLen ctx.pairs, last + 1
  ctx.pairs.data[last].symbol = symbol
  ctx.pairs.data[last].value = val

proc lookup(vm: VM, ctx: BaseContext, symbol: StringConst, create: bool): ptr Value

proc lookup(vm: VM, ctx: Context, symbol: StringConst, create: bool): ptr Value =
  for i in 0..<ctx.pairs.count:
    if ctx.pairs.data[i].symbol == symbol:
      return addr ctx.pairs.data[i].value
  if create:
    let last = ctx.pairs.count
    vm.setLen ctx.pairs, last + 1
    ctx.pairs.data[last].symbol = symbol
    ctx.pairs.data[last].value = Null
    result = addr ctx.pairs.data[last].value
  else:
    assert(false, "lookup failed: " & $symbol)

proc lookup(vm: VM, ctx: FuncContext, symbol: StringConst, create: bool): ptr Value =
  for i in 0..<ctx.data.count:
    if ctx.data[i] == symbol:
#      echo "found symbol ", symbol, " at ", i, " bp = ", vm.bp, " value = ", vm.stack.data[vm.bp + i]
      return addr vm.stack.data[vm.bp + i]
  result = vm.lookup(ctx.base.parent, symbol, create)

proc lookup(vm: VM, ctx: ReactiveContext, symbol: StringConst, create: bool): ptr Value =
  for i in 0..<ctx.closures.count:
    if ctx.closures[i].symbol == symbol:
      return ctx.closures[i].value
  # forward up
  result = vm.lookup(ctx.base.parent, symbol, create)
  if result != nil:
    let last = ctx.closures.count
    vm.setLen ctx.closures, last + 1
    ctx.closures.data[last].symbol = symbol
    ctx.closures.data[last].value = result    

proc newReactiveContext(vm: VM, parent: BaseContext): ReactiveContext =
  result = vm.allocate(ReactiveContextObj)
  init(result.base, oReactiveContext, parent)
  init(result.closures)

proc lookup(vm: VM, ctx: BaseContext, symbol: StringConst, create: bool): ptr Value =
  case ctx.obj.codeword:
  of oFuncContext: 
    result = vm.lookup(cast[FuncContext](ctx), symbol, create)
  of oContext:
    result = vm.lookup(cast[Context](ctx), symbol, create)
  of oReactiveContext:
    result = vm.lookup(cast[Context](ctx), symbol, create)
  else:
    assert false, "shit happens"

proc install(vm: VM, ctx: BaseContext) =
  ctx.parent = vm.context
  vm.context = ctx

#
# Parser
#

#
# create block or paren parsing it from givent position in the string, 
# returns index of created constant
proc parse(vm: VM, s: string, pos: var int, paren = false): Block =
  var items: Buffer[Value]
  var i = pos
  while true:
    if i >= s.len: break
    case s[i]
    of Whitespace: inc i
    of ']', ')': 
      inc i
      break
    of '[':
      inc i
      vm.add items, value(vm.parse(s, i))
    of '(':
      inc i
      vm.add items, value(vm.parse(s, i, true))
    of Digits:
      let start = i
      while i < s.len and (s[i] in Digits): inc(i)
      let iv = parseInt(s.substr(start, i-1))
      vm.add items, value(iv)
    else:
      let start = i
      while i < s.len and not (s[i] in Whitespace) and s[i] != ']' and s[i] != ')': inc(i)
      let w = substr(s, start, i-1)
      if w[^1] == ':':
        vm.add items, value(vm.newWord(vm.newString(substr(w, 0, w.len - 2)), wkSetWord))
      else:
        vm.add items, value(vm.newWord(vm.newString(w), wkWord))

  pos = i
  result = vm.newBlock(items) #if paren: newParen(items) else: newBlock(items)
  vm.clear(items)

proc parse(vm: VM, s: string): Block =
  var pos = 0
  result = vm.parse(s, pos)

#
# V M
#

proc interpretBlock(vm: VM, blk: Block)

proc push(vm: VM, value: Value) = vm.add vm.stack, value
proc peek(vm: VM): Value = vm.stack.data[vm.stack.count - 1]
proc poke(vm: VM, val: Value) = vm.stack.data[vm.stack.count - 1] = val
proc pop(vm: VM): Value = 
  dec vm.stack.count
  result = vm.stack.data[vm.stack.count]
proc drop(vm: VM) = 
  dec vm.stack.count

# proc push(vm: VM, ip: ptr Value) = vm.add vm.rstack, ip
# proc popIP(vm: VM): ptr Value = 
#   dec vm.rstack.count
#   result = vm.rstack.data[vm.stack.count]

proc showStack(vm: VM) =
  echo "STACK:"
  for i in vm.stack:
    echo i

const natives = [
  (name: "print", params: 1,  f: proc (vm: VM) = 
    echo vm.peek()
  ),
  (name: "func", params: 2,  f: proc (vm: VM) = 
    let body = cast[Block](vm.pop().asPointer)
    let locals = cast[Block](vm.peek().asPointer)
    let ctx = vm.newFuncContext(vm.context)
    for i in locals:
      let symbol = cast[Word](i.asPointer).symbol
      vm.add ctx.data, symbol
    let f = vm.newFunc(ctx.data.count, ctx, body)
    vm.poke(value(f))
  ),  
  (name: "add", params: 2,  f: proc (vm: VM) = 
    let y = vm.pop()
    let x = vm.peek()
    assert x.isNum and y.isNum
    vm.poke(Value(x.int + y.int))
  ),
  (name: "lt", params: 2,  f: proc (vm: VM) = 
    let y = vm.pop()
    let x = vm.peek()
    assert x.isNum and y.isNum
    vm.poke(value(x.int < y.int))
  ),  
  (name: "while", params: 2,  f: proc (vm: VM) = 
    let body = cast[Block](vm.pop().asPointer)
    let cond = cast[Block](vm.pop().asPointer)
    vm.push Null
    while true:
      vm.push False
      vm.interpretBlock(cond)
      if vm.pop.int == False.int:
        break
      vm.interpretBlock(body)
  ),
  (name: "make-vector!", params: 1,  f: proc (vm: VM) = 
    let init = cast[Block](vm.peek().asPointer)
    let v = vm.newVector()
    vm.add v.data, addr init.data[0], init.len
    vm.poke value(v)
  ),
  (name: "react", params: 1,  f: proc (vm: VM) = 
    let code = cast[Block](vm.peek().asPointer)
    let ctx = vm.newReactiveContext(vm.context)
    # vm.add v.data, addr init.data[0], init.len
    # vm.poke value(v)
  )
]

proc createSystem(vm: VM): Context =
  result = vm.newContext(nil)
  for f in natives:
    vm.add(result, vm.newString(f.name), value(vm.newFunc(f.params, f.f)))

proc newVM(): VM =
  result = create VMObj
  result.context = cast[BaseContext](result.createSystem())

# Interprets single command
proc interpret(vm: VM, ip: ptr Value, value: Value): ptr Value =
  template nextIP() = 
    result = cast[ptr Value](cast[int](ip) +% sizeof(Value))

  case tag(value):
  of tagInt, tagSpecial:
    vm.push(value)
    nextIP()
  of tagObj, tagConst:
    let obj = value.asPointer
    case obj.codeword
    of oString, oBlock:
      vm.push value
      nextIP()
    of oWord:
      let word = cast[Word](obj)
      case word.kind
      of wkWord:
        let resolved = vm.lookup(vm.context, word.symbol, false)
        assert resolved != nil
        result = vm.interpret(ip, resolved[])
      of wkSetWord:
        nextIP()
        result = vm.interpret(result, result[])
        let resolved = vm.lookup(vm.context, word.symbol, true)
        resolved[] = vm.peek()
    of oFunc:
      let f = cast[Function](obj)
      nextIP()
      let bp = vm.stack.count
      for i in 0..<f.base.params:
        result = vm.interpret(result, result[])
      vm.bp = bp
      let localCtx = cast[BaseContext](f.locals)
      vm.install localCtx
      vm.push Null # default result
      vm.interpretBlock(f.body)
      vm.context = vm.context.parent
    of oFuncNative:
      let f = cast[FuncNative](obj)
      nextIP()
      for i in 0..<f.base.params:
        result = vm.interpret(result, result[])
      f.f(vm)
    else:
      echo "hmmm... should I learn how to execute: ", obj.codeword

# Does NOT change stack size
proc interpretBlock(vm: VM, blk: Block) =
  var ip = addr blk.data[0]
  let last = addr blk.data[blk.len]
  while ip < last:
    vm.drop()
    ip = vm.interpret(ip, ip[])

