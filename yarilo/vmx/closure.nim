
include module

proc newFunction(gc: GC, module: ObjModule, maxSlots: int): ObjFn =
  result = cast[ObjFn](gc.allocate(TFn))
  gc.init(result.obj, okFn)

  result.module = module
  result.maxSlots = maxSlots
