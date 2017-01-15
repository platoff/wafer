

proc isUndefined(val: Value): bool = val.kind == vkUndefined
proc isFalse(val: Value): bool = val.kind == vkFalse
proc isObj(val: Value): bool = val.kind == vkObj
proc isNum(val: Value): bool = val.kind == vkNum

proc NumVal(num: float64): Value =
  result.kind = vkNum
  result.num = num

converter asVal(obj: Obj): Value =
  assert obj != nil
  result.kind = vkObj
  result.obj = obj

proc asObj(val: Value): Obj =
  assert val.kind == vkObj or val.kind == vkNull
  cast[Obj](val.obj)

proc asVal(p: ObjectType): Value = cast[Obj](p).asVal

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

var
  UndefinedVal = Value(kind: vkUndefined, obj: nil)
  FalseVal = Value(kind: vkFalse, obj: nil)
  NullVal = Value(kind: vkNull, obj: nil)
