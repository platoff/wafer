

const

# A mask that selects the sign bit.
  SIGN_BIT = 1u64 shl 63

# The bits that must be set to indicate a quiet NaN.
  QNAN = 0x7ffc000000000000u64

# Tag values for the different singleton values.
type 
  Tag = enum
    tagNAN = 0
    tagNull = 1
    tagFalse = 2
    tagTrue = 3
    tagUndefined = 4
    tagUnused5, tagUnused6, tagUnused7

const
# Singleton values.
  NullVal* = Value(QNAN or tagNull.uint64)
  FalseVal* = Value(QNAN or tagFalse.uint64)
  TrueVal* = Value(QNAN or tagTrue.uint64)
  UndefinedVal = Value(QNAN or tagUndefined.uint64)

# Masks out the tag bits used to identify the singleton value.
  MaskTag: int = 7
  
proc getTag(value: Value): Tag = Tag(int(value) and MaskTag)

# If the NaN bits are set, it's not a number.
template isNum(value: Value): bool = ((uint64(value) and QNAN) != QNAN)

# An object pointer is a NaN with a set sign bit.
template isObj*(value: Value): bool = ((uint64(value) and (QNAN or SIGN_BIT)) == (QNAN or SIGN_BIT))

template isFalse*(value: Value): bool = uint64(value) == uint64(FalseVal)
template isNull*(value: Value): bool = uint64(value) == uint64(NullVal)
template isUndefined*(value: Value): bool = uint64(value) == uint64(UndefinedVal)

# Value -> 0 or 1.
template asBool(value: Value): bool = ((value) == TrueVal)

# Value -> Obj.
template asObj*(value: Value): Obj = cast[Obj](uint64(value) and (not (SIGN_BIT or QNAN)))

converter asVal*(obj: Obj): Value {.inline.} = Value(SIGN_BIT or QNAN or cast[uint64](obj))

proc `===`(a, b: Value): bool = uint64(a) == uint64(b)

proc hash(value: Value): uint32 =
  if isObj(value):
    result = hash(asObj(value))
  else:
    var val = uint64(value)
    result = hash(cast[DoubleBits](addr val))

proc internalGetClass(vm: WrenVM, value: Value): ObjClass =
  case value.getTag:
  of tagFalse, tagTrue: vm.boolClass
  of tagNAN: vm.numClass
  of tagNull: vm.nullClass
  else: 
    assert false, "unreachable"
    nil


#// Gets the singleton type tag for a Value (which must be a singleton).
#define GET_TAG(value) ((int)((value) & MASK_TAG))
