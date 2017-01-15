

const

# A mask that selects the sign bit.
  SIGN_BIT = 1u64 shl 63

# The bits that must be set to indicate a quiet NaN.
  QNAN = 0x7ffc000000000000u64

# Tag values for the different singleton values.
  TAG_NAN = 0u64
  TAG_NULL = 1u64
  TAG_FALSE = 2u64
  TAG_TRUE = 3u64
  TAG_UNDEFINED = 4u64

# Singleton values.
  NullVal* = Value(QNAN or TAG_NULL)
  FalseVal* = Value(QNAN or TAG_FALSE)
  TrueVal* = Value(QNAN or TAG_TRUE)
  UndefinedVal = Value(QNAN or TAG_UNDEFINED)

# Masks out the tag bits used to identify the singleton value.
  MaskTag = 7u64
  
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


#// Gets the singleton type tag for a Value (which must be a singleton).
#define GET_TAG(value) ((int)((value) & MASK_TAG))
