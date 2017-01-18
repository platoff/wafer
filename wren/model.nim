
type
  FlexibleArray{.unchecked.}[T] = array[0..0, T]
  PArray*[T] = ptr FlexibleArray[T]

  Buffer*[T] = object
    data*: PArray[T]
    count: uint32
    capacity: uint32

  String = object
    buffer*: cstring
    length*: int

  SymbolTable* = Buffer[String]

when defined(NanTagging):
  type
    Value* = distinct uint64  
else:
  type
    ValueKind* = enum
      vkFalse
      vkNull
      vkNum
      vkTrue
      vkObj
      vkUndefined

    Value* = object
      case kind*: ValueKind
      of vkNum: num: float64
      else: obj*: pointer

const 
# The maximum number of temporary objects that can be made visible to the GC
# at one time.
  MaxTempRoots = 5
  MaxVariableName* = 64
# The maximum number of fields a class can have, including inherited fields.
# This is explicit in the bytecode since `CODE_CLASS` and `CODE_SUBCLASS` take
# a single byte for the number of fields. Note that it's 255 and not 256
# because creating a class takes the *number* of fields, not the *highest
# field index*.
  MaxFields* = 255

type
  ObjKind* = enum
    okObject
    okClass
    okString
    okModule
    okFn
    okClosure
    okMap
    okFiber
    okInstance
    okUpvalue

  Obj* = ptr TObj
  TObj = object
    kind*: ObjKind
    isDark: bool
    classObj*: ObjClass
    next*: Obj # The next object in the linked list of all currently allocated objects.

  ObjString* = ptr TString
  TString = object
    obj: TObj
    length: uint32
    hash: uint32
    value*: FlexibleArray[char]

  ObjInstance* = ptr TInstance
  TInstance = object
    obj*: TObj
    fields*: FlexibleArray[Value]

  MapEntry = object
    key: Value
    value: Value

  ObjMap* = ptr TMap
  TMap = object
    obj: TObj
    capacity: uint32
    count: uint32
    entries: PArray[MapEntry]
  
  ObjUpvalue* = ptr TUpvalue
  TUpvalue = object
    obj: TObj
    value*: ptr Value
    closed*: Value
    next*: ObjUpValue

  Primitive* = proc (vm: VM, args: PArray[Value]): bool {.nimcall.} 
  ForeignMethodFn* = proc (vm: pointer) {.cdecl.} 

  ObjModule* = ptr TModule
  TModule = object
    obj: TObj
    variables*: Buffer[Value]
    variableNames*: SymbolTable
    name*: ObjString

  FnDebug = object
    name*: cstring
    sourceLines*: Buffer[int]

  ObjFn* = ptr TFn
  TFn = object
    obj: TObj
    code*: Buffer[byte]
    constants*: Buffer[Value]
    module*: ObjModule
    maxSlots*: int
    numUpvalues*: int
    arity*: int
    debug*: ptr FnDebug 

  ObjClosure* = ptr TClosure
  TClosure = object
    obj: TObj
    fn*: ObjFn
    upvalues*: FlexibleArray[ObjUpvalue]

  CallFrame* = object
    ip*: PArray[byte]
    closure*: ObjClosure
    stackStart*: PArray[Value]

  ObjFiber* = ptr TFiber
  TFiber = object
    obj: TObj
    stack*: PArray[Value]
    stackTop*: PArray[Value]
    stackCapacity*: int
    frames*: PArray[CallFrame]
    numFrames*: int
    frameCapacity*: int
  # Pointer to the first node in the linked list of open upvalues that are
  # pointing to values still on the stack. The head of the list will be the
  # upvalue closest to the top of the stack, and then the list works downwards. 
    openUpvalues*: ObjUpvalue
    caller*: ObjFiber
    error*: Value
    callerIsTrying*: bool

  MethodKind* = enum
  # A primitive method implemented in C in the VM. Unlike foreign methods,
  # this can directly manipulate the fiber's stack.
    METHOD_PRIMITIVE
  # A externally-defined C method.
    METHOD_FOREIGN
  # A normal user-defined method.
    METHOD_BLOCK
  # // The special "call(...)" methods on function.
    METHOD_FN_CALL
  # No method for the given symbol.
    METHOD_NONE

  Method* = object
    case kind*: MethodKind
    of METHOD_PRIMITIVE: primitive*: Primitive
    of METHOD_FOREIGN: foreign*: ForeignMethodFn
    else: obj*: ObjClosure

  ObjClass* = ptr TClass 
  TClass = object
    obj*: TObj
    superclass*: ObjClass
    numFields*: int
    methods*: Buffer[Method]
    name*: ObjString

  VM* = ptr WrenVM
  WrenVM* = object
    boolClass*: ObjClass
    classClass*: ObjClass
    fiberClass*: ObjClass
    fnClass*: ObjClass
    listClass*: ObjClass
    mapClass*: ObjClass
    nullClass*: ObjClass
    numClass*: ObjClass
    objectClass*: ObjClass
    rangeClass*: ObjClass
    stringClass*: ObjClass

    fiber*: ObjFiber

    bytesAllocated: int
    nextGC: int
    first*: Obj

    gray: PArray[Obj]
    grayCount: int
    grayCapacity: int

    numTempRoots: int
    tempRoots: array[MaxTempRoots, Obj]

    modules*: ObjMap  
    methodNames*: SymbolTable

  # Pointer to the bottom of the range of stack slots available for use from
  # the C API. During a foreign method, this will be in the stack of the fiber
  # that is executing a method.
  #
  # If not in a foreign method, this is initially NULL. If the user requests
  # slots by calling wrenEnsureSlots(), a stack is created and this is
  # initialized.
    apiStack*: ptr Value

    compiler*: pointer

proc collectGarbage(vm: VM)

proc reallocate*(vm: VM, memory: pointer; oldSize, newSize: int): pointer {.inline.} =
  inc vm.bytesAllocated, newSize - oldSize
  when defined(DebugGCStress):
    if newSize > 0:
      gc.collectGarbage
  else:
    if newSize > 0 and vm.bytesAllocated > vm.nextGC:
      vm.collectGarbage

  realloc(memory, newSize)

proc deallocate(vm: VM, p: pointer, oldSize = 0.Natural) {.inline.} =
  discard vm.reallocate( p, oldSize, 0)

proc allocate(vm: VM, T: typedesc): ptr T  {.inline.} =
  result = cast[ptr T](vm.reallocate(nil, 0, sizeof T))
  zeroMem(result, sizeof T)

proc allocate(vm: VM, T: typedesc, n: int): PArray[T] {.inline.} =
  cast[PArray[T]](vm.reallocate(nil, 0, n * sizeof T))

proc allocateFlex(vm: VM, T: typedesc, I: typedesc, n: int): ptr T {.inline.} =
  cast[ptr T](vm.reallocate(nil, 0, sizeof(T) + n * sizeof I))

#
#
#

proc inc*[T](pa: var PArray[T], n = 1) =
  pa = cast[PArray[T]](cast[int](pa) +% (sizeof(T) * n))

proc dec*[T](pa: var PArray[T], n = 1) =
  pa = cast[PArray[T]](cast[int](pa) -% (sizeof(T) * n))

#
# Buffers
#

proc init*(buf: var Buffer) =
  buf.data = nil
  buf.count = 0
  buf.capacity = 0

template len*[T](buf: Buffer[T]): int = int(buf.count)

proc clear*(vm: VM, buf: var Buffer) {.inline.} =
  vm.deallocate(buf.data, int(buf.capacity))
  buf.init

proc `[]`*[T](buf: Buffer[T], i: int): T {.inline.} =
  assert i < int(buf.count)
  buf.data[i]
  
proc `[]=`*[T](buf: var Buffer[T], i: int, val: T) {.inline.} = 
  assert i < int(buf.count)
  buf.data[i] = val

# From: http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2Float
# proc powerOf2Ceil(n: uint32): uint32 =
#   result = n - 1
#   result = result or (n shr 1)
#   result = result or (n shr 2)
#   result = result or (n shr 4)
#   result = result or (n shr 8)
#   result = result or (n shr 16)
#   result = result + 1

proc fill[T](vm: VM, buffer: var Buffer[T], value: T, count: int) {.inline.} =
  let newCount = int(buffer.count) + count
  var capacity = int(buffer.capacity)
  if capacity < newCount:  
    while true:
      if capacity == 0: capacity = 4
      else: capacity = capacity * 2
      if capacity >= newCount:
        break
    buffer.data = 
      cast[PArray[T]](
        vm.reallocate(buffer.data, int(buffer.capacity) * sizeof(T), capacity * sizeof(T)))
  for i in 0..<count:
    buffer.data[buffer.count] = value
    inc buffer.count

proc add*[T](vm: VM, buffer: var Buffer[T], val: T) {.inline.} = vm.fill(buffer, val, 1)

#
# Symbol Table
# 

proc find*(table: SymbolTable, name: cstring, length: int): int =
  for i in 0..<int(table.count):
    if table.data[i].length == length and
      equalMem(table.data[i].buffer, name, length):
        return i
  result = -1

proc add*(vm: VM, symbols: var SymbolTable, name: cstring, length: int): int =
  var symbol: String
  symbol.buffer = cast[cstring](vm.allocate(char, length + 1))
  copyMem(symbol.buffer, name, length)
  symbol.buffer[length] = char(0)
  symbol.length = (int)length
  vm.add(symbols, symbol)
  result = int(symbols.count) - 1

proc ensure*(vm: VM, symbols: var SymbolTable, name: cstring, length: int): int =
  # See if the symbol is already defined.
  let existing = find(symbols, name, length)
  if existing != -1: 
    result = existing
  else:
    result = vm.add(symbols, name, length)

##
## R U N T I M E    V A L U E S
##

type
  ObjectType = ObjClosure | ObjFn | ObjString | ObjMap | ObjModule | ObjClass | Obj |
    ObjInstance

# Object Classes downcast
converter objectDowncast*(obj: ObjectType): Obj {.inline.} = cast[Obj](obj)

type
  DoubleBits = ptr array[2, uint32]

proc slosh(u32: uint32): uint32 = 
  result = u32
  result = result xor ((result shr 20) xor (result shr 12))
  result = result xor ((result shr 7) xor (result shr 4))

proc hash(bits: DoubleBits): uint32 = slosh(bits[0] xor bits[1])

proc hash(obj: Obj): uint32

when defined(NanTagging):
  include value64
else:
  include valued

template val*(b: bool): Value = 
  if b: TrueVal else: FalseVal

template val*(i: int): Value = val(i.float64)

proc vmcast*[T: ObjectType](val: Value): T =
  assert val.isObj
  cast[T](val.asObj)

proc isString*(value: Value): bool {.inline.} = 
  value.isObj and value.asObj.kind == okString
proc isClass*(value: Value): bool {.inline.} = 
  value.isObj and value.asObj.kind == okClass
proc isInstance*(value: Value): bool {.inline.} = 
  value.isObj and value.asObj.kind == okInstance
proc isClosure*(value: Value): bool {.inline.} = 
  value.isObj and value.asObj.kind == okClosure

proc getClass*(vm: VM, value: Value): ObjClass {.inline.} =
  if value.isNum:
    vm.numClass
  elif value.isObj:
    value.asObj.classObj
  else:
    vm.internalGetClass(value)

proc `==`*(a, b: Value): bool = 
  if a === b:
    result = true
  else:
  # If we get here, it's only possible for two heap-allocated immutable objects
  # to be equal.
    if not isObj(a) or not isObj(b):
      result = false
    else:
      let 
        aObj = a.asObj
        bObj = b.asObj
  
      # Must be the same kind.
      if aObj.kind != bObj.kind:
        result = false
      else:
        case aObj.kind
        of okString: 
          let 
            aString = cast[ObjString](aObj)
            bString = cast[ObjString](bObj)

          result = aString.length == bString.length and
             aString.hash == bString.hash and
             equalMem(addr aString.value[0], addr bString.value[0], int(aString.length))
        else:
        # All other types are only equal if they are same, which they aren't if
        # we get here.
          result = false

proc hash(obj: Obj): uint32 =
  case obj.kind
  of okString:
    cast[ObjString](obj).hash
  of okClass:
    # Classes just use their name.
    cast[ObjClass](obj).name.hash
    # Allow bare (non-closure) functions so that we can use a map to find
    # existing constants in a function's constant table. This is only used
    # internally. Since user code never sees a non-closure function, they
    # cannot use them as map keys.
  of okFN:
    let fn = cast[ObjFn](obj)
    slosh(fn.arity.uint32 * 31) xor slosh(fn.code.count)
  else:
    assert false, "Only immutable objects can be hashed: " & $obj.kind
    0

proc initObj(vm: VM, obj: var TObj, kind: ObjKind, classObj: ObjClass) {.inline.} =
  obj.kind = kind
  obj.isDark = false
  obj.classObj = classObj
  obj.next = vm.first
  vm.first = addr obj

##
## S T R I N G
##

# Calculates and stores the hash code for [string].
# FNV-1a hash. See: http://www.isthe.com/chongo/tech/comp/fnv/
proc hashString(str: ObjString) =
  var hash = 2166136261u
  for i in 0..<str.length:
    hash = hash xor uint(str.value[i])
    hash *= 16777619

  str.hash = uint32(hash)

proc allocateString(vm: VM, length: int): ObjString {.inline.} =
  result = vm.allocateFlex(TString, char, length + 1)
  vm.initObj(result.obj, okString, vm.stringClass)
  result.length = uint32(length)
  result.value[length] = char(0)

proc newString*(vm: VM, text: cstring, length: int): ObjString  =
  assert(length == 0 or text != nil, "Unexpected null string.")
  result = vm.allocateString(length)
  if length > 0 and text != nil:
    copyMem(addr result.value[0], text, length)
  hashString(result)

proc newString*(vm: VM, text: cstring): ObjString =
  vm.newString(text, text.len)

template c_value*(str: ObjString): cstring = cast[cstring](addr str.value[0])
template len*(str: ObjString): int = int(str.length)

proc `$$`*(str: cstring): String = 
  result.buffer = str
  result.length = str.len

proc `$$`*(str: ObjString): String = 
  result.buffer = str.c_value
  result.length = str.length.int

proc stringFormat*(vm: VM, format: cstring, values: varargs[String,`$$`]): ObjString  =
  # Calculate the length of the result string. Do this up front so we can
  # create the final string with a single allocation.
  var totalLength = 0
  var i = 0
  for c in format:
    case c:
    of '$':
      inc totalLength, values[i].length
      inc i

    else:
      # Any other character is interpreted literally.
      inc totalLength

  # Concatenate the string.
  result = vm.allocateString(totalLength)

  i = 0
  var dst = 0
  for c in format:
    case c:
    of '$':
      copyMem(addr result.value[dst], values[i].buffer, values[i].length)
      inc dst, values[i].length
      inc i

    else:
      # Any other character is interpreted literally.
      result.value[dst] = c
      inc dst

  hashString(result)

##
## M A P S
##

const
  MinCapacity = 16
  GrowFactor = 2
  MapLoadPercent = 75

type
  PMapEntry = ptr MapEntry

proc newMap*(vm: VM): ObjMap =
  result = cast[ObjMap](vm.allocate(TMap))
  vm.initObj(result.obj, okMap, vm.mapClass)

proc newMap*(vm: VM, map: var ObjMap) =
  map = vm.newMap()

# Looks for an entry with [key] in an array of [capacity] [entries].

# If found, sets [result] to point to it and returns `true`. Otherwise,
# returns `false` and points [result] to the entry where the key/value pair
# should be inserted.
proc findEntry(entries: PArray[MapEntry], capacity: int, 
  key: Value, resultEntry: var PMapEntry): bool =
  # If there is no entry array (an empty map), we definitely won't find it.
  if capacity == 0:
    return false
  
  # Figure out where to insert it in the table. Use open addressing and
  # basic linear probing.
  let startIndex = int(hash(key)) mod capacity
  var index = startIndex
  
  # If we pass a tombstone and don't end up finding the key, its entry will
  # be re-used for the insert.
  var tombstone: PMapEntry = nil
  
  # Walk the probe sequence until we've tried every slot.
  while true:
    let entry = addr entries[index]
    
    if isUndefined(entry.key):
      # If we found an empty slot, the key is not in the table. If we found a
      # slot that contains a deleted key, we have to keep looking.
      if isFalse(entry.value):
        # We found an empty slot, so we've reached the end of the probe
        # sequence without finding the key. If we passed a tombstone, then
        # that's where we should insert the item, otherwise, put it here at
        # the end of the sequence.
        resultEntry = if tombstone != nil: tombstone else: entry
        return false
      else:
        # We found a tombstone. We need to keep looking in case the key is
        # after it, but we'll use this entry as the insertion point if the
        # key ends up not being found.
        if tombstone == nil:
          tombstone = entry
    elif entry.key == key:
      # We found the key.
      resultEntry = entry
      return true

    # Try the next slot.
    index = (index + 1) mod capacity
    if index == startIndex:
      break
  
  # If we get here, the table is full of tombstones. Return the first one we
  # found.
  assert(tombstone != nil, "Map should have tombstones or empty entries.")
  resultEntry = tombstone

# Inserts [key] and [value] in the array of [entries] with the given
# [capacity].
# 
# Returns `true` if this is the first time [key] was added to the map.
proc insertEntry(entries: PArray[MapEntry], capacity: int; key, value: Value): bool {.inline.} =
  var entry: PMapEntry
  if findEntry(entries, capacity, key, entry):
    # Already present, so just replace the value.
    entry.value = value
    result = false;
  else:
    assert(entry != nil, "Should ensure capacity before inserting.")
    entry.key = key
    entry.value = value
    result = true;

# Updates [map]'s entry array to [capacity].
proc resizeMap(vm: VM, map: var TMap, capacity: int) {.inline.} =
  # Create the new empty hash table.
  let entries = vm.allocate(MapEntry, capacity)
  for i in 0..<capacity:
    entries[i].key = UndefinedVal
    entries[i].value = FalseVal

  # Re-add the existing entries.
  if map.capacity > 0u32:
    for i in 0..<map.capacity:
      let entry = addr map.entries[i]
      # Don't copy empty entries or tombstones.
      if entry.key.isUndefined: 
        continue
      discard insertEntry(entries, capacity, entry.key, entry.value)

  # Replace the array.
  vm.deallocate(map.entries, int(map.capacity))
  map.entries = entries
  map.capacity = uint32(capacity)

proc `[]`*(map: ObjMap, key: Value): Value {.inline.} =
  var entry: PMapEntry
  if findEntry(map.entries, int(map.capacity), key, entry): entry.value
  else: UndefinedVal

proc mapSet*(vm: VM, map: ObjMap; key, value: Value) {.inline.} =
  # If the map is getting too full, make room first.
  if map.count + 1 > map.capacity * MapLoadPercent div 100:
    # Figure out the new hash table size.
    let capacity = max(int(map.capacity) * GrowFactor, MinCapacity)
    vm.resizeMap(map[], capacity)

  if insertEntry(map.entries, int(map.capacity), key, value):
    inc map.count

##
## M O D U L E S 
##

proc newModule*(vm: VM, name: ObjString): ObjModule  =
  result = cast[ObjModule](vm.allocate(TModule))
  vm.initObj(result.obj, okModule, nil)
  result.name = name


proc newInstance*(vm: VM, classObj: ObjClass): ObjInstance =
  result = allocateFlex(vm, TInstance, Value, classObj.numFields)
  initObj(vm, result.obj, okInstance, classObj)

  # Initialize fields to null.
  for i in 0..<classObj.numFields:
    result.fields[i] = NullVal

proc newUpvalue*(vm: VM, value: ptr Value): ObjUpvalue =
  result = allocate(vm, TUpvalue)

  # Upvalues are never used as first-class objects, so don't need a class.
  initObj(vm, result.obj, okUpvalue, nil)

  result.value = value
  result.closed = NullVal
  result.next = nil


##
## F U N C T I O N    A N D    C L O S U R E
##

proc newFunction*(vm: VM, module: ObjModule, maxSlots: int): ObjFn =
  result = vm.allocate(TFn)
  vm.initObj(result.obj, okFn, vm.fnClass)

  result.debug = vm.allocate(FnDebug)
  # debug->name = NULL;
  # wrenIntBufferInit(&debug->sourceLines);
  
#  wrenValueBufferInit(&fn->constants);
#  wrenByteBufferInit(&fn->code);
  result.module = module
  result.maxSlots = maxSlots
  result.numUpvalues = 0
  result.arity = 0
#  fn->debug = debug;

proc bindName*(vm: VM, fn: ObjFn, name: cstring, length: int) =
  fn.debug.name = cast[cstring](vm.allocate(char, length + 1))
  copyMem(fn.debug.name, name, length)
  fn.debug.name[length] = '\0'

proc newClosure*(vm: VM, fn: ObjFn): ObjClosure =
  result = vm.allocateFlex(TClosure, ObjUpvalue, fn.numUpvalues)
  vm.initObj(result.obj, okClosure, vm.fnClass)
  result.fn = fn
  # Clear the upvalue array. We need to do this in case a GC is triggered
  # after the closure is created but before the upvalue array is populated.
  for i in 0..<fn.numUpvalues:
    result.upvalues[i] = nil

##
## F I B E R
##

const 
# The number of call frames initially allocated when a fiber is created. Making
# this smaller makes fibers use less memory (at first) but spends more time
# reallocating when the call stack grows.
  INITIAL_CALL_FRAMES = 4

# Adds a new [CallFrame] to [fiber] invoking [closure] whose stack starts at
# [stackStart].
proc appendCallFrame(vm: VM, fiber: ObjFiber,
                     closure: ObjClosure, stackStart: PArray[Value]) =
  # The caller should have ensured we already have enough capacity.
  assert (fiber.frameCapacity > fiber.numFrames, "No memory for call frame.")
  
  let frame = addr fiber.frames[fiber.numFrames]
  inc fiber.numFrames
  frame.stackStart = stackStart
  frame.closure = closure
  frame.ip = closure.fn.code.data

proc reset(vm: VM, fiber: ObjFiber, closure: ObjClosure) =
  # Push the stack frame for the function.
  fiber.stackTop = fiber.stack
  fiber.openUpvalues = nil
  fiber.caller = nil
  fiber.error = NullVal
  fiber.callerIsTrying = false
  fiber.numFrames = 0

  # Initialize the first call frame.
  if closure != nil:
    vm.appendCallFrame(fiber, closure, fiber.stack)

proc newFiber*(vm: VM, closure: ObjClosure): ObjFiber =
  # Allocate the arrays before the fiber in case it triggers a GC.
  let frames = vm.allocate(CallFrame, INITIAL_CALL_FRAMES)
  
  # Add one slot for the unused implicit receiver slot that the compiler
  # assumes all functions have.
  let stackCapacity = if closure == nil:
        1
      else: 
        1024 #(int)powerOf2Ceil((uint32)closure.fn.maxSlots + 1)
  let stack = vm.allocate(Value, stackCapacity)
  
  result = vm.allocate(TFiber)
  vm.initObj(result.obj, okFiber, vm.fiberClass)
  result.frames = frames
  result.frameCapacity = INITIAL_CALL_FRAMES
  result.stack = stack
  result.stackCapacity = stackCapacity
  vm.reset(result, closure)

##
## C L A S S E S
##

proc newSingleClass*(vm: VM, numFields: int, name: ObjString): ObjClass  =
  result = vm.allocate(TClass)
  initObj(vm, result.obj, okClass, nil)
  result.numFields = numFields
  result.name = name

proc bindMethod*(vm: VM, classObj: ObjClass, symbol: int, meth: Method) =
  # Make sure the buffer is big enough to contain the symbol's index.
  if symbol >= classObj.methods.len:
    var noMethod: Method
    noMethod.kind = METHOD_NONE
    vm.fill(classObj.methods, noMethod, symbol - classObj.methods.len + 1)

  classObj.methods[symbol] = meth

proc bindSuperclass*(vm: VM, subclass: ObjClass, superclass: ObjClass) =
  assert(superclass != nil, "Must have superclass.")

  subclass.superclass = superclass

  # Include the superclass in the total number of fields.
  if subclass.numFields != -1:
    inc subclass.numFields, superclass.numFields
  else:
    assert(superclass.numFields == 0,
           "A foreign class cannot inherit from a class with fields.")

  # Inherit methods from its superclass.
  for i in 0..<superclass.methods.len:
    vm.bindMethod(subclass, i, superclass.methods.data[i])

proc pushRoot*(vm: VM, obj: Obj)  =
  assert vm.numTempRoots < MaxTempRoots
  vm.tempRoots[vm.numTempRoots] = cast[Obj](obj)
  inc vm.numTempRoots

proc popRoot*(vm: VM, obj: Obj)  = 
  dec vm.numTempRoots
  assert vm.tempRoots[vm.numTempRoots].pointer == obj.pointer

proc newClass*(vm: VM, superclass: ObjClass, numFields: int, name: ObjString): ObjClass =
  # Create the metaclass.
  let metaclassName = stringFormat(vm, "$ metaclass", name)
  pushRoot(vm, metaclassName)

  let metaclass = newSingleClass(vm, 0, metaclassName)
  metaclass.obj.classObj = vm.classClass

  popRoot(vm, metaclassName)

  # Make sure the metaclass isn't collected when we allocate the class.
  pushRoot(vm, metaclass)

  # Metaclasses always inherit Class and do not parallel the non-metaclass
  # hierarchy.
  bindSuperclass(vm, metaclass, vm.classClass)

  result = newSingleClass(vm, numFields, name)

  # Make sure the class isn't collected while the inherited methods are being
  # bound.
  pushRoot(vm, result)

  result.obj.classObj = metaclass
  bindSuperclass(vm, result, superclass)

  popRoot(vm, result)
  popRoot(vm, metaclass)


##
## G A R B A G E    C O L L E C T O R
##


proc grayObj(vm: VM, obj: Obj) =
  if obj == nil: return

  # Stop if the object is already darkened so we don't get stuck in a cycle.
  if obj.isDark: return

  # It's been reached.
  obj.isDark = true

  # Add it to the gray list so it can be recursively explored for
  # more marks later.
  if vm.grayCount >= vm.grayCapacity:
    vm.grayCapacity = vm.grayCount * 2
    vm.gray = cast[PArray[Obj]](realloc(vm.gray, vm.grayCapacity * sizeof(Obj)))

  vm.gray[vm.grayCount] = obj
  inc vm.grayCount

proc grayValue(vm: VM, value: Value) =
  if isObj(value):
    vm.grayObj(value.asObj)

proc blacken(vm: VM, module: ObjModule) =
  # Top-level variables.
  for i in 0..<module.variables.len:
    vm.grayValue(module.variables.data[i])

  grayObj(vm, cast[Obj](module.name))

  # Keep track of how much memory is still in use.
  inc vm.bytesAllocated, sizeof(TModule)
  # TODO: Track memory for symbol table and buffer.

proc blacken(vm: VM, str: ObjString) =
  # Keep track of how much memory is still in use.
  inc vm.bytesAllocated,  sizeof(TString) + str.len + 1

proc blackenObject(vm: VM, obj: Obj) =
  # Traverse the object's fields.
  case obj.kind:
    #of okClass: vm.blacken(cast[ObjClass](obj))
    of okString: vm.blacken(cast[ObjString](obj))
    of okModule: vm.blacken(cast[ObjModule](obj))
    else: assert false, "not implemented"

proc blackenObjects(vm: VM) =
  while vm.grayCount > 0:
    # Pop an item from the gray stack.
    dec vm.grayCount
    let obj = vm.gray[vm.grayCount]
    blackenObject(vm, obj)

proc freeObj(vm: VM, obj: Obj) =
  case obj.kind:
  of okMap:
    vm.deallocate(cast[ObjMap](obj).entries, 0)
  else:
    assert false, "not implemented"

proc collectGarbage(vm: VM) =
  vm.bytesAllocated = 0
  vm.grayObj(cast[Obj](vm.modules))
  for i in 0..<vm.numTempRoots:
    vm.grayObj(vm.tempRoots[i])
  vm.grayObj(cast[Obj](vm.fiber))

  # Now that we have grayed the roots, do a depth-first search over all of the
  # reachable objects.
  blackenObjects(vm)

  # Collect the white objects.
  var obj = addr vm.first
  while obj[] != nil:
    if not obj[].isDark:
      # This object wasn't reached, so remove it from the list and free it.
      let unreached = obj[]
      obj[] = unreached.next
      vm.freeObj(unreached)
    else:
      obj[].isDark = false
      obj = addr obj[].next

  # +100 here because the configuration gives us the *additional* size of
  # the heap relative to the in-use memory, while heapScalePercent is the
  # *total* size of the heap relative to in-use.
#  vm.nextGC = vm.bytesAllocated * (100 + vm.config.heapGrowthPercent) / 100
  vm.nextGC = vm.bytesAllocated * (100 + 10) div 100
#  if vm.nextGC < vm->config.minHeapSize) vm->nextGC = vm->config.minHeapSize;

##
## V M
##

const 
# The maximum number of arguments that can be passed to a method. Note that
# this limitation is hardcoded in other places in the VM, in particular, the
# `CODE_CALL_XX` instructions assume a certain maximum number.
  MAX_PARAMETERS* = 16

# The maximum name of a method, not including the signature. This is an
# arbitrary but enforced maximum just so we know how long the method name
# strings need to be in the parser.
  MAX_METHOD_NAME* = 64

# The maximum length of a method signature. Signatures look like:
#
#     foo        // Getter.
#     foo()      // No-argument method.
#     foo(_)     // One-argument method.
#     foo(_,_)   // Two-argument method.
#     init foo() // Constructor initializer.
#
# The maximum signature length takes into account the longest method name, the
# maximum number of parameters with separators between them, "init ", and "()".
  MAX_METHOD_SIGNATURE* = (MAX_METHOD_NAME + (MAX_PARAMETERS * 2) + 6)

# The maximum number of module-level variables that may be defined at one time.
# This limitation comes from the 16 bits used for the arguments to
# `CODE_LOAD_MODULE_VAR` and `CODE_STORE_MODULE_VAR`.
  MaxModuleVars = 65536
# The maximum number of arguments that can be passed to a method. Note that
# this limitation is hardcoded in other places in the VM, in particular, the
# `CODE_CALL_XX` instructions assume a certain maximum number.  
  #MaxParameters* = 16
# The maximum name of a method, not including the signature. This is an
# arbitrary but enforced maximum just so we know how long the method name
# strings need to be in the parser.
#  MaxMethodName* = 64

proc init*(vm: VM, initialHeapSize: int) =
  vm.grayCount = 0
  vm.grayCapacity = 4
  vm.nextGC = initialHeapSize
  vm.numTempRoots = 0
  vm.newMap(vm.modules) # TODO: mapClass is nil, as in original... hope it's OK

proc defineVariable*(vm: VM, module: ObjModule, name: cstring,
                     length: int, value: Value): int =
  assert module != nil
  if module.variables.len == MaxModuleVars: 
    return -2

  if value.isObj:
    vm.pushRoot(value.asObj)  # TODO: do we need this here?

  # See if the variable is already explicitly or implicitly declared.
  result = module.variableNames.find(name, length)
  if result == -1:
    # Brand new variable.
    result = vm.add(module.variableNames, name, length)
    vm.add(module.variables, value)
  elif isNum(module.variables[result]):
    ## An implicitly declared variable's value will always be a number. Now we
    ## have a real definition.
    module.variables[result] = value
  else:
    # Already explicitly declared.
    result = -1

  if value.isObj:
    vm.popRoot(value.asObj)

proc declareVariable*(vm: VM, module: ObjModule, name: cstring,
                        length: int, line: int): int =
  if module.variables.len == MAX_MODULE_VARS: 
    return -2

  # Implicitly defined variables get a "value" that is the line where the
  # variable is first used. We'll use that later to report an error on the
  # right line.
  vm.add(module.variables, line.val)
  return vm.add(module.variableNames, name, length)
