
type
  FlexibleArray{.unchecked.}[T] = array[0..0, T]
  HugeArray*[T] = ptr FlexibleArray[T]

  Buffer[T] = object
    data: HugeArray[T]
    count: uint32
    capacity: uint32

  String = object
    buffer*: cstring
    length*: int

  SymbolTable* = Buffer[String]

# proc isEqual[T](a, b: var FlexibleArray[T], length: int): bool =
#   for i in 0..<length:
#     if a[i] != b[i]:
#       return false
#   result = true





#
# Allocator helpers
#

proc deallocate[A](a: A, p: pointer, oldSize: int) {.inline.} = 
  discard a.reallocate(p, oldSize, 0)

proc allocate[A](a: A, T: typedesc): ptr T {.inline.} = 
  result = cast[ptr T](a.reallocate(nil, 0, sizeof T))
  zeroMem(result, sizeof T)

proc allocate[A](a: A, T: typedesc, n: int): HugeArray[T] {.inline.} = 
  cast[HugeArray[T]](a.reallocate(nil, 0, n * sizeof T))

proc allocateFlex[A](a: A, T: typedesc, I: typedesc, n: int): ptr T {.inline.} = 
  cast[ptr T](a.reallocate(nil, 0, sizeof(T) + n * sizeof I))

#
# Buffers
#

proc init(buf: var Buffer) =
  buf.data = nil
  buf.count = 0
  buf.capacity = 0

proc len*[T](buf: Buffer[T]): int {.inline.} = int(buf.count)

proc clear[A](a: A, buf: var Buffer) =
  a.deallocate(buf.data, int(buf.capacity))
  buf.init

proc `[]`*[T](buf: Buffer[T], i: int): T {.inline.} =
  assert i < int(buf.count)
  buf.data[i]
  
proc `[]=`*[T](buf: var Buffer[T], i: int, val: T) = 
  assert i < int(buf.count)
  buf.data[i] = val

proc data*[T](buf: Buffer[T]): HugeArray[T] {.inline.} = buf.data

# From: http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2Float
proc powerOf2Ceil(n: int): int =
  result = n - 1
  result = result or (n shr 1)
  result = result or (n shr 2)
  result = result or (n shr 4)
  result = result or (n shr 8)
  result = result or (n shr 16)
  result = result + 1

proc fill[A,T](allocator: A, buffer: var Buffer[T], value: T, count: int) =
  let newCount = int(buffer.count) + count
  if int(buffer.capacity) < newCount:
    let capacity = powerOf2Ceil(newCount)
    buffer.data = 
      cast[HugeArray[T]](allocator.reallocate(buffer.data, int(buffer.capacity) * sizeof(T), capacity * sizeof(T)))
    for i in 0..<count:
      buffer.data[buffer.count] = value
      inc buffer.count

proc add*[A,T](allocator: A, buffer: var Buffer[T], val: T) = allocator.fill(buffer, val, 1)

#
# Symbol Table
# 

proc find*(table: SymbolTable, name: cstring, length: int): int =
  for i in 0..<int(table.count):
    if table.data[i].length == length and
      equalMem(table.data[i].buffer, name, length):
        return i
  result = -1

proc add*[A](allocator: A, symbols: var SymbolTable, name: cstring, length: int): int =
  var symbol: String
  symbol.buffer = cast[cstring](allocator.allocate(char, length + 1))
  copyMem(symbol.buffer, name, length)
  symbol.buffer[length] = char(0)
  symbol.length = (int)length
  allocator.add(symbols, symbol)
  result = int(symbols.count) - 1

proc ensure*[GC](gc: GC, symbols: var SymbolTable, name: cstring, length: int): int =
  # See if the symbol is already defined.
  let existing = find(symbols, name, length)
  if existing != -1: 
    result = existing
  else:
    result = add(gc, symbols, name, length)

##
## R U N T I M E    O B J E C T S
##

when defined(NanTagging):
  type
    Value* = distinct uint64  
else:
  type
    ValueKind = enum
      vkFalse
      vkNull
      vkNum
      vkTrue
      vkObj
      vkUndefined

    Value* = object
      case kind: ValueKind
      of vkNum: num: float64
      else: obj: pointer

type
  ObjKind* = enum
    okObject
    okClass
    okString
    okModule
    okFn
    okClosure
    okMap

  Obj* = ptr TObj
  TObj = object
    kind: ObjKind
    isDark: bool
    classObj: ObjClass
    next: Obj # The next object in the linked list of all currently allocated objects.

  ObjString* = ptr TString
  TString = object
    obj: TObj
    length: uint32
    hash: uint32
    value: FlexibleArray[char]

  MapEntry = object
    key: Value
    value: Value

  ObjMap* = ptr TMap
  TMap = object
    obj: TObj
    capacity: uint32
    count: uint32
    entries: HugeArray[MapEntry]
  
  ObjUpvalue = ptr TUpvalue
  TUpvalue = object
    obj: TObj
    value: ptr Value
    closed: Value
    next: ObjUpValue

  Primitive* = proc (vm: pointer, args: HugeArray[Value]): bool {.nimcall.} 
  ForeignMethodFn* = proc (vm: pointer) {.cdecl.} 

  ObjModule* = ptr TModule
  TModule = object
    obj: TObj
    variables*: Buffer[Value]
    variableNames*: SymbolTable
    name*: ObjString

  ObjFn* = ptr TFn
  TFn = object
    obj: TObj
    code*: Buffer[byte]
    constants*: Buffer[Value]
    module: ObjModule
    maxSlots: int
    numUpvalues*: int
    arity: int

  ObjClosure* = ptr TClosure
  TClosure = object
    obj: TObj
    fn*: ObjFn
    upvalues: FlexibleArray[ObjUpvalue]

  CallFrame = object
    ip: ptr byte
    closure: ObjClosure
    stackStart: ptr Value

  ObjFiber* = ptr TFiber
  TFiber = object
    obj: TObj
    stack: HugeArray[Value]
    stackTop: ptr Value
    stackCapacity: int
    frames: HugeArray[CallFrame]
    numFrames: int
    frameCapacity: int
    openUpvalues: HugeArray[ObjUpvalue]
    caller: ObjFiber
    error*: Value
    callerIsTrying: bool

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
    obj: TObj
    superclass*: ObjClass
    numFields*: int
    methods: Buffer[Method]
    name*: ObjString

  ObjectType = ObjString | ObjMap | ObjModule | ObjClass

  DoubleBits = ptr array[2, uint32]

proc hash(bits: DoubleBits): uint32 =
  result = bits[0] xor bits[1]  
  result = result xor ((result shr 20) xor (result shr 12))
  result = result xor ((result shr 7) xor (result shr 4))

proc hash(obj: Obj): uint32

# Object Classes downcast
converter asObj*(obj: ObjectType): Obj {.inline.} = cast[Obj](obj)

when defined(NanTagging):
  include value64
else:
  include valued

proc asVal*(b: bool): Value = 
  if b: TrueVal else: FalseVal

proc asString*(obj: Obj): ObjString =
  assert obj == nil or obj.kind == okString
  result = cast[ObjString](obj)

proc asString*(val: Value): ObjString = 
  assert val.isObj
  val.asObj.asString

proc asClosure*(obj: Obj): ObjClosure {.inline.} = 
  assert obj == nil or obj.kind == okClosure
  result = cast[ObjClosure](obj)

proc asClosure*(val: Value): ObjClosure {.inline.} = 
  assert val.isObj
  val.asObj.asClosure

proc asFn*(obj: Obj): ObjFn {.inline.} = 
  assert obj == nil or obj.kind == okFn
  result = cast[ObjFn](obj)

proc asFn*(val: Value): ObjFn {.inline.} = 
  assert val.isObj
  val.asObj.asFn

proc asModule*(obj: Obj): ObjModule =
  assert obj == nil or obj.kind == okModule, if obj == nil: "nil" else: $obj.kind
  result = cast[ObjModule](obj)

proc asModule*(val: Value): ObjModule = val.asObj.asModule

proc asMap(obj: Obj): ObjMap =
  assert obj == nil or obj.kind == okMap, if obj == nil: "nil" else: $obj.kind
  result = cast[ObjMap](obj)
proc asMap(val: Value): ObjMap = val.asObj.asMap

proc isString*(value: Obj): bool {.inline.} = value.kind == okString
proc isString*(value: Value): bool {.inline.} = value.isObj and value.asObj.isString
proc isClass*(value: Obj): bool {.inline.} = value.kind == okClass
proc isClass*(value: Value): bool {.inline.} = value.isObj and value.asObj.isClass

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
            aString = aObj.asString
            bString = bObj.asString

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
  else:
    assert false, "Only immutable objects can be hashed."
    0

proc initObj[GC](gc: GC, obj: var TObj, kind: ObjKind) {.inline.} =
  obj.kind = kind
  obj.isDark = false
  obj.classObj = gc.getClass(kind)
  obj.next = gc.addObj(addr obj)


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

proc allocateString[A](a: A, length: int): ObjString {.inline.} =
  result = a.allocateFlex(TString, char, length + 1)
  a.initObj(result.obj, okString)
  result.length = uint32(length)
  result.value[length] = char(0)

proc newString*[A](a: A, text: cstring, length: int): ObjString {.inline.} =
  assert(length == 0 or text != nil, "Unexpected null string.")
  result = a.allocateString(length)
  if length > 0 and text != nil:
    copyMem(addr result.value[0], text, length)
  hashString(result)

template newString*(a: untyped, text: cstring): ObjString = 
  a.newString(text, text.len)

proc value*(str: ObjString): cstring {.inline.} = cast[cstring](addr str.value[0])
proc len*(str: ObjString): int {.inline.} = int(str.length)

template `$$`*(str: cstring): pointer = str.pointer
template `$$`*(str: ObjString): pointer = str.pointer

proc stringFormat*[GC](vm: GC, format: cstring, values: varargs[pointer,`$$`]): ObjString =
  # Calculate the length of the result string. Do this up front so we can
  # create the final string with a single allocation.
  var totalLength = 0
  var i = 0
  for c in format:
    case c:
    of '$':
      let str = cast[cstring](values[i])
      inc totalLength, str.len
      inc i

    of '@':
      inc totalLength, cast[ObjString](values[i]).len
      inc i

    else:
      # Any other character is interpreted literally.
      inc totalLength

  # Concatenate the string.
  result = allocateString(vm, totalLength)

  i = 0
  var dst = 0
  for c in format:
    case c:
    of '$':
      let str = cast[cstring](values[i])
      let length = str.len
      copyMem(addr result.value[dst], str, length)
      inc dst, length
      inc i

    of '@':
      let str = asString(cast[Obj](values[i]))
      copyMem(addr result.value[dst], addr str.value[0], str.length)
      inc dst, str.len
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

proc newMap*[GC](gc: GC): ObjMap {.inline.} =
  result = cast[ObjMap](gc.allocate(TMap))
  gc.initObj(result.obj, okMap)

proc newMap*[GC](gc: GC, map: var ObjMap) {.inline.} =
  map = gc.newMap()

# Looks for an entry with [key] in an array of [capacity] [entries].

# If found, sets [result] to point to it and returns `true`. Otherwise,
# returns `false` and points [result] to the entry where the key/value pair
# should be inserted.
proc findEntry(entries: HugeArray[MapEntry], capacity: int, 
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
proc insertEntry(entries: HugeArray[MapEntry], capacity: int; key, value: Value): bool {.inline.} =
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
proc resizeMap[GC](gc: GC, map: var TMap, capacity: int) {.inline.} =
  # Create the new empty hash table.
  let entries = gc.allocate(MapEntry, capacity)
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
  gc.deallocate(map.entries, int(map.capacity))
  map.entries = entries
  map.capacity = uint32(capacity)

proc `[]`*(map: ObjMap, key: Value): Value {.inline.} =
  var entry: PMapEntry
  if findEntry(map.entries, int(map.capacity), key, entry): entry.value
  else: UndefinedVal

proc mapSet*[GC](gc: GC, map: ObjMap; key, value: Value) {.inline.} =
  # If the map is getting too full, make room first.
  if map.count + 1 > map.capacity * MapLoadPercent div 100:
    # Figure out the new hash table size.
    let capacity = max(int(map.capacity) * GrowFactor, MinCapacity)
    gc.resizeMap(map[], capacity)

  if insertEntry(map.entries, int(map.capacity), key, value):
    inc map.count
  
##
## M O D U L E S 
##

proc newModule*[GC](gc: GC, name: ObjString): ObjModule  =
  result = cast[ObjModule](gc.allocate(TModule))
  gc.initObj(result.obj, okModule)
  result.name = name

##
## C L A S S E S
##

proc newSingleClass*[GC](vm: GC, numFields: int, name: ObjString): ObjClass =
  result = vm.allocate(TClass)
  initObj(vm, result.obj, okClass)
  result.numFields = numFields
  result.name = name

proc bindMethod*[GC](gc: GC, classObj: ObjClass, symbol: int, meth: Method) =
  # Make sure the buffer is big enough to contain the symbol's index.
  if symbol >= classObj.methods.len:
    var noMethod: Method
    noMethod.kind = METHOD_NONE
    gc.fill(classObj.methods, noMethod,
                         symbol - classObj.methods.len + 1)

  classObj.methods[symbol] = meth

