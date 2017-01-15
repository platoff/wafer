
#import value

include value

const 
# The maximum number of temporary objects that can be made visible to the GC
# at one time.
  MaxTempRoots = 5

type
  GC* = ptr GCObj
  GCObj* = object
    classes*: array[ObjKind, ObjClass]

    bytesAllocated: int
    nextGC: int
    first: Obj

    gray: ptr Obj
    grayCount: int
    grayCapacity: int

    numTempRoots: int
    tempRoots: array[MaxTempRoots, Obj]

proc init*(gc: var GCObj, initialHeapSize: int) =
  gc.grayCount = 0
  gc.grayCapacity = 4
  gc.nextGC = initialHeapSize
  gc.numTempRoots = 0

proc getClass*(gc: GC, kind: ObjKind): ObjClass {.inline.} = gc.classes[kind]

# notify GC on new object has been created, return previously created one
proc addObj*(gc: GC, obj: Obj): Obj {.inline.} =
  result = gc.first
  gc.first = obj

proc pushRoot*(gc: GC, obj: Obj) {.inline.} =
  assert gc.numTempRoots < MaxTempRoots
  gc.tempRoots[gc.numTempRoots] = obj
  inc gc.numTempRoots

proc popRoot*(gc: GC, obj: Obj) {.inline.} = 
  dec gc.numTempRoots
  assert gc.tempRoots[gc.numTempRoots] == obj

proc collectGarbage(gc: GC) =
  discard

proc reallocate*(gc: GC, memory: pointer; oldSize, newSize: int): pointer {.inline.} =
  inc gc.bytesAllocated, newSize - oldSize
  when defined(DebugGCStress):
    if newSize > 0:
      gc.collectGarbage
  else:
    if newSize > 0 and gc.bytesAllocated > gc.nextGC:
      gc.collectGarbage

  realloc(memory, newSize)
