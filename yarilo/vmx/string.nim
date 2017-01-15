
include value

# Calculates and stores the hash code for [string].
# FNV-1a hash. See: http://www.isthe.com/chongo/tech/comp/fnv/
proc hashString(str: ObjString) =
  var hash = 2166136261u
  for i in 0..<str.length:
    hash = hash xor uint(str.value[i])
    hash *= 16777619

  str.hash = uint32(hash)

proc allocateString[A](a: A, length: int): ObjString =
  result = a.allocateFlex(TString, char, length + 1)
  a.initObj(result.obj, okString)
  result.length = uint32(length)
  result.value[length] = char(0)

proc newString[A](a: A, text: cstring, length: int): ObjString =
  assert(length == 0 or text != nil, "Unexpected null string.")
  result = a.allocateString(length)
  if length > 0 and text != nil:
    copyMem(addr result.value[0], text, length)
  hashString(result)

template newString(a: untyped, text: cstring): ObjString = 
  a.newString(text, text.len)

