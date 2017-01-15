
include module

const 
# The maximum number of temporary objects that can be made visible to the GC
# at one time.
  MaxTempRoots = 5

type
  GC = ptr GCObj
  GCObj = object
    classes: array[ObjKind, ObjClass]

    bytesAllocated: int
    nextGC: int
    first: Obj

    gray: ptr Obj
    grayCount: int
    grayCapacity: int

    numTempRoots: int
    tempRoots: array[MaxTempRoots, Obj]

proc init(gc: var GCObj, initialHeapSize: int) =
  gc.grayCount = 0
  gc.grayCapacity = 4
  gc.nextGC = initialHeapSize
  gc.numTempRoots = 0

proc pushRoot(gc: GC, obj: Obj) =
  assert gc.numTempRoots < MaxTempRoots
  gc.tempRoots[gc.numTempRoots] = obj
  inc gc.numTempRoots

proc popRoot(gc: GC, obj: Obj) = 
  dec gc.numTempRoots
  assert gc.tempRoots[gc.numTempRoots] == obj

proc collectGarbage(gc: GC) =
  discard

proc initObj[GC](gc: GC, obj: var TObj, kind: ObjKind) =
  obj.kind = kind
  obj.isDark = false
  obj.classObj = gc.classes[kind]
  obj.next = gc.first
  gc.first = addr obj

proc reallocate[GC](gc: GC, memory: pointer; oldSize, newSize: int): pointer =
  inc gc.bytesAllocated, newSize - oldSize
  when defined(DebugGCStress):
    if newSize > 0:
      gc.collectGarbage
  else:
    if newSize > 0 and gc.bytesAllocated > gc.nextGC:
      gc.collectGarbage

  realloc(memory, newSize)
