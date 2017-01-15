
include utils

when defined(NanTagging):
  type
    Value = distinct uint64  
else:
  type
    ValueKind = enum
      vkFalse
      vkNull
      vkNum
      vkTrue
      vkObj
      vkUndefined

    Value = object
      case kind: ValueKind
      of vkNum: num: float64
      else: obj: pointer

type
  ObjKind = enum
    okClass
    okString
    okModule
    okFn
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
  
  ObjModule = ptr TModule
  TModule = object
    obj: TObj
    variables: Buffer[Value]
    variableNames: SymbolTable
    name: ObjString

  ObjFn = ptr TFn
  TFn = object
    obj: TObj
    code: Buffer[byte]
    constants: Buffer[Value]
    module: ObjModule
    maxSlots: int
    numUpvalues: int
    arity: int

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

#
# Object Classes downcast
#
converter asObj(obj: ObjectType): Obj = cast[Obj](obj)

when defined(NanTagging):
  include value64
else:
  include valued

proc asString(obj: Obj): ObjString =
  assert obj == nil or obj.kind == okString
  result = cast[ObjString](obj)

proc asString(val: Value): ObjString = 
  assert val.isObj
  val.asObj.asString

proc asModule(obj: Obj): ObjModule =
  assert obj == nil or obj.kind == okModule, if obj == nil: "nil" else: $obj.kind
  result = cast[ObjModule](obj)

proc asModule(val: Value): ObjModule = val.asObj.asModule

proc asMap(obj: Obj): ObjMap =
  assert obj == nil or obj.kind == okMap, if obj == nil: "nil" else: $obj.kind
  result = cast[ObjMap](obj)
proc asMap(val: Value): ObjMap = val.asObj.asMap

proc `==`(a, b: Value): bool = 
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

