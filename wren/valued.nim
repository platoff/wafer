

proc isUndefined*(val: Value): bool = val.kind == vkUndefined
proc isFalse*(val: Value): bool = val.kind == vkFalse
proc isObj*(val: Value): bool = val.kind == vkObj
proc isNum*(val: Value): bool = val.kind == vkNum
proc isNull*(val: Value): bool = val.kind == vkNull

proc val*(num: float64): Value =
  result.kind = vkNum
  result.num = num

proc val*(obj: Obj): Value {.inline.} =
  assert obj != nil
  result.kind = vkObj
  result.obj = obj

proc asObj*(val: Value): Obj =
  assert val.kind == vkObj
  cast[Obj](val.obj)

proc asNum*(val: Value): float64 =
  assert val.kind == vkNum
  val.num

proc `===`(a, b: Value): bool =
  if a.kind != b.kind: false
  elif a.kind == vkNum: a.num == b.num
  else: a.obj == b.obj

proc hash(num: float64): uint32 = 
  var val = num
  hash(cast[DoubleBits](addr val))

proc hash(val: Value): uint32 =
  case val.kind:
  of vkFalse: 0u32
  of vkNull: 1
  of vkNum: hash(val.num)
  of vkTrue: 2
  of vkObj: hash(val.asObj)
  else: 0u32

proc internalGetClass(vm: VM, value: Value): ObjClass =
  case value.kind:
  of vkFalse, vkTrue: vm.boolClass
  of vkNull: vm.nullClass
  of vkNum: vm.numClass
  else: 
    assert false, "unreachable"
    nil

var
  UndefinedVal* = Value(kind: vkUndefined, obj: nil)
  FalseVal* = Value(kind: vkFalse, obj: nil)
  TrueVal* = Value(kind: vkTrue, obj: nil)
  NullVal* = Value(kind: vkNull, obj: nil)
