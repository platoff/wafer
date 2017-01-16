
import utils

const 
# The maximum number of temporary objects that can be made visible to the GC
# at one time.
  WrenMaxTempRoots = 5

type
  ValueKind = enum
    vkFalse
    vkNull
    vkNum
    vkTrue
    vkObj
    vkUndefined

  PValue = ptr Value
  Value* = object
    case kind: ValueKind
    of vkNum: num: float64
    else: obj: pointer

type
  ObjKind* = enum
    okClass
    okModule
    okMap

  Obj* = ptr YObj
  YObj = object
    kind: ObjKind
    isDark: bool
    classObj: ObjClass
    next: Obj # The next object in the linked list of all currently allocated objects.

  ObjString* = distinct pointer #ptr YString
  # YString = object
  #   obj: YObj

  ObjModule* = ptr YModule
  YModule = object
    obj: YObj
    variables: Buffer[Value]
    variableNames: SymbolTable
    name: ObjString

  Method = object
    m: int

  ObjClass* = ptr YClass
  YClass = object
    obj: YObj
    superclass: ObjClass
    numfields: int
  # The table of methods that are defined in or inherited by this class.
  # Methods are called by symbol, and the symbol directly maps to an index in
  # this table. This makes method calls fast at the expense of empty cells in
  # the list for methods the class doesn't support.
  
  # You can think of it as a hash table that never has collisions but has a
  # really low load factor. Since methods are pretty small (just a type and a
  # pointer), this should be a worthwhile trade-off.
    methods: Buffer[Method]
    name: ObjString

  PMapEntry = ptr MapEntry
  MapEntry = object
  # The entry's key, or UNDEFINED_VAL if the entry is not in use.
    key: Value

  # The value associated with the key. If the key is UNDEFINED_VAL, this will
  # be false to indicate an open available entry or true to indicate a
  # tombstone -- an entry that was previously in use but was then deleted.
    value: Value

# A hash table mapping keys to values.

# We use something very simple: open addressing with linear probing. The hash
# table is an array of entries. Each entry is a key-value pair. If the key is
# the special UNDEFINED_VAL, it indicates no value is currently in that slot.
# Otherwise, it's a valid key, and the value is the value associated with it.

# When entries are added, the array is dynamically scaled by GROW_FACTOR to
# keep the number of filled slots under MAP_LOAD_PERCENT. Likewise, if the map
# gets empty enough, it will be resized to a smaller array. When this happens,
# all existing entries are rehashed and re-added to the new array.

# When an entry is removed, its slot is replaced with a "tombstone". This is an
# entry whose key is UNDEFINED_VAL and whose value is TRUE_VAL. When probing
# for a key, we will continue past tombstones, because the desired key may be
# found after them if the key that was removed was part of a prior collision.
# When the array gets resized, all tombstones are discarded.

  ObjMap* = ptr YMap
  YMap = object
    obj: YObj
    capacity: uint32
    count: uint32
    entries: HugeArray[MapEntry]

  DoubleBits = ptr array[2, uint32]

  VM* = ptr WrenVM
  WrenVM = object
    classes: array[ObjKind, ObjClass]

    bytesAllocated: int
    nextGC: int
    first: Obj

    gray: ptr Obj
    grayCount: int
    grayCapacity: int

    numTempRoots: int
    tempRoots: array[WrenMaxTempRoots, Obj]
    
    modules: ObjMap  
    methodNames: SymbolTable
  
var
  UndefinedVal = Value(kind: vkUndefined, obj: nil)
  FalseVal = Value(kind: vkFalse, obj: nil)
  NullVal = Value(kind: vkNull, obj: nil)

type
  ObjectType = ObjModule | ObjString

proc value*(obj: ObjectType): Value = 
  result.kind = vkObj
  result.obj = obj

converter asValue*(obj: ObjModule): Value = value(obj)

proc asObj*(val: Value): Obj = 
  assert val.kind == vkObj
  cast[Obj](val.obj)
proc asModule*(val: Value): ObjModule = cast[ObjModule](val.asObj)
proc asString*(val: Value): ObjString = cast[ObjString](val.asObj)

converter asObj(obj: ObjectType): Obj = cast[Obj](obj)

proc isUndefined*(val: Value): bool = val.kind == vkUndefined
proc isFalse*(val: Value): bool = val.kind == vkFalse
proc isObj*(val: Value): bool = val.kind == vkObj
proc isNum*(val: Value): bool = val.kind == vkNum

proc collectGarbage(vm: VM)  

proc reallocate(vm: VM, memory: pointer; oldSize, newSize: int): pointer =
  # If new bytes are being allocated, add them to the total count. If objects
  # are being completely deallocated, we don't track that (since we don't
  # track the original size). Instead, that will be handled while marking
  # during the next GC.
  inc vm.bytesAllocated, newSize - oldSize
  when defined(WrenDebugGCStress):
    if newSize > 0:
      vm.collectGarbage
  else:
    if newSize > 0 and vm.bytesAllocated > vm.nextGC:
      vm.collectGarbage

  realloc(memory, newSize)
    
# proc fill[T](vm: VM, buffer: var Buffer[T], data: T, count: static[int]) =
#   let newCount = int(buffer.count) + count
#   if int(buffer.capacity) < newCount:
#     let capacity = powerOf2Ceil(newCount)
#     buffer.data = 
#       cast[HugeArray[T]](vm.reallocate(buffer.data, int(buffer.capacity) * sizeof(T), capacity * sizeof(T)))
#     for i in 0..<count:
#       buffer.data[buffer.count] = data
#       inc buffer.count

# proc add[T](vm: VM, buffer: var Buffer[T], val: T) = 
#   fill(vm, buffer, val, 1)

# proc find*(table: SymbolTable, name: cstring, length: int): int =
#   for i in 0..<int(table.count):
#     if table.data[i].length == length and
#       equalMem(table.data[i].buffer, name, length):
#         return i
#   result = -1

# proc add(vm: VM, symbols: var SymbolTable, name: cstring, length: int): int =
#   var symbol: String
#   symbol.buffer = cast[cstring](allocate(vm, char, length + 1))
#   copyMem(symbol.buffer, name, length)
#   symbol.buffer[length] = '\0';
#   symbol.length = (int)length;
  
#   add(vm, symbols, symbol)
#   result = int(symbols.count) - 1

#
# M A P
#

const
  MinCapacity = 16
  GrowFactor = 2
  MapLoadPercent = 75

proc `==`(a, b: Value): bool = 
  echo "not implemented"

proc hash(bits: DoubleBits): uint32 {.inline.} =
  result = bits[0] xor bits[1]  
  result = result xor ((result shr 20) xor (result shr 12))
  result = result xor ((result shr 7) xor (result shr 4))

proc hash(num: float64): uint32 = 
  var val = num
  hash(cast[DoubleBits](addr val))

proc hash(obj: Obj): uint32 =
  echo "not implemented"

proc hashValue(val: Value): uint32 =
  when defined(NanTagging):
    echo "not implemented"
  else:
    case val.kind:
    of vkFalse: 0u32
    of vkNull: 1
    of vkNum: hash(val.num)
    of vkTrue: 2
    of vkObj: hash(val.asObj)
    else: 0u32

proc internalNewObj(vm: VM, T: typedesc): ptr T =
  result = vm.allocate T
  zeroMem(result, sizeof(T))
  let obj = cast[Obj](result)
  when T is YModule:
    obj.kind = okModule
  elif T is YMap:
    obj.kind = okMap
  else:
    {.error: "1".}
  obj.classObj = vm.classes[obj.kind]
  obj.next = vm.first
  vm.first = obj

proc newObj[T](vm: VM, obj: var ptr T) =
  obj = vm.internalNewObj T

template createObj(vm: VM, obj: untyped, T: typedesc, s: untyped) =
  block:
    var obj: ptr T
    vm.newObj obj
    assert(vm.numTempRoots < WrenMaxTempRoots, "Too many temporary roots.")
    vm.tempRoots[vm.numTempRoots] = obj
    inc vm.numTempRoots
    s
    dec vm.numTempRoots


#
# Module
#

# The maximum number of module-level variables that may be defined at one time.
# This limitation comes from the 16 bits used for the arguments to
# `CODE_LOAD_MODULE_VAR` and `CODE_STORE_MODULE_VAR`.
const 
  MaxModuleVars = 65536

proc defineVariable(vm: VM, module: ObjModule, name: cstring,
                       length: int, value: Value): int =
  if module.variables.len == MaxModuleVars: 
    return -2

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

# Looks up the previously loaded module with [name].
#
# Returns `NULL` if no module with that name has been loaded.
proc getModule(vm: VM, name: Value): ObjModule =
  let moduleValue = vm.modules[name]
  if not moduleValue.isUndefined: 
    result = moduleValue.asModule

proc loadModule(vm: VM, name: Value, source: cstring) =
  var module = vm.getModule name
  if module == nil:
    vm.createObj(m, YModule):
      m.name = name.asString
      vm.mapSet(vm.modules, name, m.value)
      module = m

    # Implicitly import the core module.
    let coreModule = getModule(vm, NullVal)
    for i in 0..<int(coreModule.variables.len):
      discard defineVariable(vm, module,
                          coreModule.variableNames[i].buffer,
                          coreModule.variableNames[i].length,
                          coreModule.variables[i])

  #let fn = vm.wrenCompile(module, source, false, true)

#
# VM
#

proc collectGarbage(vm: VM) =
  discard

proc initializeCore(vm: VM) =
  vm.createObj(module, YModule):
    vm.mapSet(vm.modules, NullVal, module)

proc newVM(initialHeapSize: int): VM =
  result = create WrenVM
  result.grayCount = 0
  result.grayCapacity = 4
  result.nextGC = initialHeapSize
  result.newObj(result.modules) # TODO: mapClass is nil, as in original... hope it's OK
  result.initializeCore

var 
  vm = newVM(65536)

vm.loadModule(NullVal, "xx")

