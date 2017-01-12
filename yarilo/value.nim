
type
  ValueKind = enum
    vkFalse
    vkNull
    vkNum
    vkTrue
    vkObj
    vkUndefined

  #PValue = ptr Value
  Value* = object
    case kind: ValueKind
    of vkNum: num: float64
    else: obj: Obj

  ObjKind* = enum
    okClass
    okString
    okModule
    okMap

  Obj* = ptr TObj
  ObjClass* = distinct Obj
  TObj* = object
    kind: ObjKind
    isDark: bool
    classObj: ObjClass
    next: Obj # The next object in the linked list of all currently allocated objects.

  ObjString* = distinct Obj
  ObjMap* = distinct Obj
  ObjModule* = distinct Obj

  ObjectType* = ObjString | ObjMap | ObjModule

proc `==`*(a, b: ObjModule): bool =
  cast[pointer](a) == cast[pointer](b)

var
  UndefinedVal* = Value(kind: vkUndefined, obj: nil)
  FalseVal* = Value(kind: vkFalse, obj: nil)
  NullVal* = Value(kind: vkNull, obj: nil)

proc isUndefined*(val: Value): bool = val.kind == vkUndefined
proc isFalse*(val: Value): bool = val.kind == vkFalse
proc isObj*(val: Value): bool = val.kind == vkObj
proc isNum*(val: Value): bool = val.kind == vkNum

proc asVal*(p: ObjectType): Value =
  result.kind = vkObj
  result.obj = Obj(p)

proc asObj*(val: Value): Obj =
  assert val.kind == vkObj or val.kind == vkNull
  val.obj

proc asString*(val: Value): ObjString =
  let obj = val.asObj
  assert obj == nil or obj.kind == okString
  result = ObjString(obj)

proc asModule*(val: Value): ObjModule =
  let obj = val.asObj
  assert obj == nil or obj.kind == okModule
  result = ObjModule(obj)

proc `==`*(a, b: Value): bool = 
  echo "not implemented"

type
  DoubleBits = ptr array[2, uint32]

proc hash(bits: DoubleBits): uint32 {.inline.} =
  result = bits[0] xor bits[1]  
  result = result xor ((result shr 20) xor (result shr 12))
  result = result xor ((result shr 7) xor (result shr 4))

proc hash(num: float64): uint32 = 
  var val = num
  hash(cast[DoubleBits](addr val))

proc hash(obj: Obj): uint32 =
  echo "not implemented"

proc hashValue*(val: Value): uint32 =
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

const 
# The maximum number of temporary objects that can be made visible to the GC
# at one time.
  WrenMaxTempRoots = 5

import utils

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

{.experimental.} 
proc `=destroy`*[T](r: var Rooted[T]) =
  echo "destroyed!"

proc init*(gc: GC, obj: var TObj, kind: ObjKind) =
  obj.kind = kind
  obj.isDark = false
  obj.classObj = gc.classes[kind]
  obj.next = gc.first
  gc.first = addr obj

proc root*[T: ObjectType](gc: GC, obj: T): Rooted[T] =
  let tempRoot = acquireRoot(gc)
  tempRoot.obj = (Obj)obj
  result = cast[Rooted[T]](tempRoot)
  
proc collectGarbage(gc: GC) =
  discard

proc reallocate*(gc: GC, memory: pointer; oldSize, newSize: int): pointer =
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