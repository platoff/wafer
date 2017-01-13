
include utils

type
  ValueKind = enum
    vkFalse
    vkNull
    vkNum
    vkTrue
    vkObj
    vkUndefined

when defined(NanTagging):
  type
    Value = uint64  
else:
  type
    Value = object
      case kind: ValueKind
      of vkNum: num: float64
      else: obj: pointer

type
  ObjKind = enum
    okClass
    okString
    okModule
    okMap

  Obj = ptr TObj
  TObj = object
    kind: ObjKind
    isDark: bool
    classObj: ObjClass
    next: Obj # The next object in the linked list of all currently allocated objects.

  ObjString = ptr TString
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
  

  ObjModule* = ptr TModule
  TModule = object
    obj: TObj
    variables: Buffer[Value]
    variableNames: SymbolTable
    name: ObjString

  ObjClass = ptr TClass 
  TClass = object
    obj: TObj

  ObjectType = ObjString | ObjMap | ObjModule | ObjClass

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

proc asString(val: Value): ObjString =
  let obj = val.asObj
  assert obj == nil or obj.kind == okString
  result = cast[ObjString](obj)

proc asModule(val: Value): ObjModule =
  let obj = val.asObj
  assert obj == nil or obj.kind == okModule
  result = cast[ObjModule](obj)

proc `==`(a, b: Value): bool = 
  if a === b:
    result = true
  else:
    echo " == is not fully implemented."

proc hash(obj: Obj): uint32 =
  echo "not implemented hash(Obj)"

const 
# The maximum number of temporary objects that can be made visible to the GC
# at one time.
  WrenMaxTempRoots = 5

type
  Rooted*[T: ObjectType] = ptr T
  PTempRoot = ptr TempRoot
  TempRoot = object
    obj: Obj
    next: PTempRoot

  GC* = ptr GCObj
  GCObj* = object
    classes: array[ObjKind, ObjClass]

    bytesAllocated: int
    nextGC: int
    first: Obj

    gray: ptr Obj
    grayCount: int
    grayCapacity: int
  
    freeRoots: PTempRoot
    busyRoots: PTempRoot
    tempRoots: array[WrenMaxTempRoots, TempRoot]

proc init*(gc: var GCObj, initialHeapSize: int) =
  gc.grayCount = 0
  gc.grayCapacity = 4
  gc.nextGC = initialHeapSize

  gc.busyRoots = nil
  gc.freeRoots = addr gc.tempRoots[0]
  for i in 0..<(WrenMaxTempRoots-1):
    gc.tempRoots[i].next = addr gc.tempRoots[i+1]
  gc.tempRoots[WrenMaxTempRoots-1].next = nil

proc acquireRoot(gc: GC): PTempRoot =
  result = gc.freeRoots
  assert result != nil
  gc.freeRoots = result.next
  result.next = gc.busyRoots
  gc.busyRoots = result

proc root[T: ObjectType](gc: GC, obj: T): Rooted[T] =
  let tempRoot = acquireRoot(gc)
  tempRoot.obj = cast[Obj](obj)
  result = cast[Rooted[T]](tempRoot)

proc release[T](gc: GC, r: Rooted[T]): T =
  let tempRoot = cast[PTempRoot](r)
  gc.busyRoots = tempRoot.next
  tempRoot.next = gc.freeRoots
  gc.freeRoots = tempRoot.next 
  result = r[]

proc init*(gc: GC, obj: var TObj, kind: ObjKind) =
  obj.kind = kind
  obj.isDark = false
  obj.classObj = gc.classes[kind]
  obj.next = gc.first
  gc.first = addr obj
  
proc collectGarbage(gc: GC) =
  discard

proc reallocate(gc: GC, memory: pointer; oldSize, newSize: int): pointer =
  # If new bytes are being allocated, add them to the total count. If objects
  # are being completely deallocated, we don't track that (since we don't
  # track the original size). Instead, that will be handled while marking
  # during the next GC.
  inc gc.bytesAllocated, newSize - oldSize
  when defined(WrenDebugGCStress):
    if newSize > 0:
      gc.collectGarbage
  else:
    if newSize > 0 and gc.bytesAllocated > gc.nextGC:
      gc.collectGarbage

  realloc(memory, newSize)