
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

const 
# The maximum number of temporary objects that can be made visible to the GC
# at one time.
  MaxTempRoots = 5

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
    classObj*: ObjClass
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

  Primitive* = proc (vm: var WrenVM, args: HugeArray[Value]): bool {.nimcall.} 
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
    obj*: TObj
    superclass*: ObjClass
    numFields*: int
    methods: Buffer[Method]
    name*: ObjString

  VM* = ptr WrenVM
  WrenVM* = object
    boolClass: ObjClass
    classClass: ObjClass
    fiberClass: ObjClass
    fnClass: ObjClass
    listClass: ObjClass
    mapClass: ObjClass
    nullClass: ObjClass
    numClass: ObjClass
    objectClass*: ObjClass
    rangeClass: ObjClass
    stringClass: ObjClass

    fiber*: ObjFiber

    bytesAllocated: int
    nextGC: int
    first: Obj

    gray: HugeArray[Obj]
    grayCount: int
    grayCapacity: int

    numTempRoots: int
    tempRoots: array[MaxTempRoots, Obj]

    modules*: ObjMap  
    methodNames*: SymbolTable

#
# Allocator
#

proc collectGarbage(vm: var WrenVM)

proc reallocate*(vm: var WrenVM, memory: pointer; oldSize, newSize: int): pointer {.inline.} =
  inc vm.bytesAllocated, newSize - oldSize
  when defined(DebugGCStress):
    if newSize > 0:
      gc.collectGarbage
  else:
    if newSize > 0 and vm.bytesAllocated > vm.nextGC:
      vm.collectGarbage

  realloc(memory, newSize)

proc deallocate(vm: var WrenVM, p: pointer, oldSize: int) {.inline.} = 
  discard vm.reallocate(p, oldSize, 0)

proc allocate(vm: var WrenVM, T: typedesc): ptr T {.inline.} = 
  result = cast[ptr T](vm.reallocate(nil, 0, sizeof T))
  zeroMem(result, sizeof T)

proc allocate(vm: var WrenVM, T: typedesc, n: int): HugeArray[T] {.inline.} = 
  cast[HugeArray[T]](vm.reallocate(nil, 0, n * sizeof T))

proc allocateFlex(vm: var WrenVM, T: typedesc, I: typedesc, n: int): ptr T {.inline.} = 
  cast[ptr T](vm.reallocate(nil, 0, sizeof(T) + n * sizeof I))

#
# Buffers
#

proc init(buf: var Buffer) =
  buf.data = nil
  buf.count = 0
  buf.capacity = 0

proc len*[T](buf: Buffer[T]): int {.inline.} = int(buf.count)

proc clear(vm: var WrenVM, buf: var Buffer) =
  vm.deallocate(buf.data, int(buf.capacity))
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

proc fill[T](vm: var WrenVM, buffer: var Buffer[T], value: T, count: int) =
  let newCount = int(buffer.count) + count
  if int(buffer.capacity) < newCount:
    let capacity = powerOf2Ceil(newCount)
    buffer.data = 
      cast[HugeArray[T]](vm.reallocate(buffer.data, int(buffer.capacity) * sizeof(T), capacity * sizeof(T)))
    for i in 0..<count:
      buffer.data[buffer.count] = value
      inc buffer.count

proc add*[T](vm: var WrenVM, buffer: var Buffer[T], val: T) = vm.fill(buffer, val, 1)

#
# Symbol Table
# 

proc find*(table: SymbolTable, name: cstring, length: int): int =
  for i in 0..<int(table.count):
    if table.data[i].length == length and
      equalMem(table.data[i].buffer, name, length):
        return i
  result = -1

proc add*(vm: var WrenVM, symbols: var SymbolTable, name: cstring, length: int): int =
  var symbol: String
  symbol.buffer = cast[cstring](vm.allocate(char, length + 1))
  copyMem(symbol.buffer, name, length)
  symbol.buffer[length] = char(0)
  symbol.length = (int)length
  vm.add(symbols, symbol)
  result = int(symbols.count) - 1

proc ensure*(vm: var WrenVM, symbols: var SymbolTable, name: cstring, length: int): int =
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
  DoubleBits = ptr array[2, uint32]

proc hash(bits: DoubleBits): uint32 =
  result = bits[0] xor bits[1]  
  result = result xor ((result shr 20) xor (result shr 12))
  result = result xor ((result shr 7) xor (result shr 4))

proc hash(obj: Obj): uint32

when defined(NanTagging):
  include value64
else:
  include valued

proc asVal*(b: bool): Value = 
  if b: TrueVal else: FalseVal


type
  ObjectType = ObjClosure | ObjFn | ObjString | ObjMap | ObjModule | ObjClass | Obj

# Object Classes downcast
proc asObj*(obj: ObjectType): Obj {.inline.} = cast[Obj](obj)

converter asVal*(obj: ObjectType): Value {.inline.} = 
  obj.asObj.asVal

proc vmcast*[T: ObjectType](val: Value): T =
  assert val.isObj
  let obj = val.asObj
  assert obj == nil or obj.kind == okString
  result = cast[T](obj)

proc isString*(value: Value): bool {.inline.} = 
  value.isObj and value.asObj.kind == okString
proc isClass*(value: Value): bool {.inline.} = 
  value.isObj and value.asObj.kind == okClass

proc getClass*(vm: WrenVM, value: Value): ObjClass {.inline.} =
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
  else:
    assert false, "Only immutable objects can be hashed."
    0

proc initObj(vm: var WrenVM, obj: var TObj, kind: ObjKind, classObj: ObjClass) {.inline.} =
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

proc allocateString(vm: var WrenVM, length: int): ObjString {.inline.} =
  result = vm.allocateFlex(TString, char, length + 1)
  vm.initObj(result.obj, okString, vm.stringClass)
  result.length = uint32(length)
  result.value[length] = char(0)

proc newString*(vm: var WrenVM, text: cstring, length: int): ObjString {.inline.} =
  assert(length == 0 or text != nil, "Unexpected null string.")
  result = vm.allocateString(length)
  if length > 0 and text != nil:
    copyMem(addr result.value[0], text, length)
  hashString(result)

template newString*(a: untyped, text: cstring): ObjString = 
  a.newString(text, text.len)

proc c_value*(str: ObjString): cstring {.inline.} = cast[cstring](addr str.value[0])
proc len*(str: ObjString): int {.inline.} = int(str.length)

template `$$`*(str: cstring): pointer = str.pointer
template `$$`*(str: ObjString): pointer = str.pointer

proc stringFormat*(vm: var WrenVM, format: cstring, values: varargs[pointer,`$$`]): ObjString =
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
      let str = cast[ObjString](values[i])
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

proc newMap*(vm: var WrenVM): ObjMap {.inline.} =
  result = cast[ObjMap](vm.allocate(TMap))
  vm.initObj(result.obj, okMap, vm.mapClass)

proc newMap*(vm: var WrenVM, map: var ObjMap) {.inline.} =
  map = vm.newMap()

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
proc resizeMap(vm: var WrenVM, map: var TMap, capacity: int) {.inline.} =
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

proc mapSet*(vm: var WrenVM, map: ObjMap; key, value: Value) {.inline.} =
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

proc newModule*(vm: var WrenVM, name: ObjString): ObjModule  =
  result = cast[ObjModule](vm.allocate(TModule))
  vm.initObj(result.obj, okModule, nil)
  result.name = name

##
## C L A S S E S
##

proc newSingleClass*(vm: var WrenVM, numFields: int, name: ObjString): ObjClass =
  result = vm.allocate(TClass)
  initObj(vm, result.obj, okClass, nil)
  result.numFields = numFields
  result.name = name

proc bindMethod*(vm: var WrenVM, classObj: ObjClass, symbol: int, meth: Method) =
  # Make sure the buffer is big enough to contain the symbol's index.
  if symbol >= classObj.methods.len:
    var noMethod: Method
    noMethod.kind = METHOD_NONE
    vm.fill(classObj.methods, noMethod,
                         symbol - classObj.methods.len + 1)

  classObj.methods[symbol] = meth

##
## G A R B A G E    C O L L E C T O R
##

proc pushRoot*(vm: var WrenVM, obj: ObjectType) {.inline.} =
  assert vm.numTempRoots < MaxTempRoots
  vm.tempRoots[vm.numTempRoots] = cast[Obj](obj)
  inc vm.numTempRoots

proc popRoot*(vm: var WrenVM, obj: ObjectType) {.inline.} = 
  dec vm.numTempRoots
  assert vm.tempRoots[vm.numTempRoots].pointer == obj.pointer

proc grayObj(vm: var WrenVM, obj: Obj) =
  if obj == nil: return

  # Stop if the object is already darkened so we don't get stuck in a cycle.
  if obj.isDark: return

  # It's been reached.
  obj.isDark = true

  # Add it to the gray list so it can be recursively explored for
  # more marks later.
  if vm.grayCount >= vm.grayCapacity:
    vm.grayCapacity = vm.grayCount * 2
    vm.gray = cast[HugeArray[Obj]](realloc(vm.gray, vm.grayCapacity * sizeof(Obj)))

  vm.gray[vm.grayCount] = obj
  inc vm.grayCount

proc grayValue(vm: var WrenVM, value: Value) =
  if isObj(value):
    vm.grayObj(value.asObj)

proc blacken(vm: var WrenVM, module: ObjModule) =
  # Top-level variables.
  for i in 0..<module.variables.len:
    vm.grayValue(module.variables.data[i])

  grayObj(vm, cast[Obj](module.name))

  # Keep track of how much memory is still in use.
  inc vm.bytesAllocated, sizeof(TModule)
  # TODO: Track memory for symbol table and buffer.

proc blacken(vm: var WrenVM, str: ObjString) =
  # Keep track of how much memory is still in use.
  inc vm.bytesAllocated,  sizeof(TString) + str.len + 1

proc blackenObject(vm: var WrenVM, obj: Obj) =
  # Traverse the object's fields.
  case obj.kind:
    #of okClass: vm.blacken(cast[ObjClass](obj))
    of okString: vm.blacken(cast[ObjString](obj))
    of okModule: vm.blacken(cast[ObjModule](obj))
    else: assert false, "not implemented"

proc blackenObjects(vm: var WrenVM) =
  while vm.grayCount > 0:
    # Pop an item from the gray stack.
    dec vm.grayCount
    let obj = vm.gray[vm.grayCount]
    blackenObject(vm, obj)

proc freeObj(vm: var WrenVM, obj: Obj) =
  case obj.kind:
  of okMap:
    vm.deallocate(cast[ObjMap](obj).entries, 0)
  else:
    assert false, "not implemented"

proc collectGarbage(vm: var WrenVM) =
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

proc init*(vm: var WrenVM, initialHeapSize: int) =
  vm.grayCount = 0
  vm.grayCapacity = 4
  vm.nextGC = initialHeapSize
  vm.numTempRoots = 0
  vm.newMap(vm.modules) # TODO: mapClass is nil, as in original... hope it's OK
