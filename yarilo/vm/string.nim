
include value

# Calculates and stores the hash code for [string].
# FNV-1a hash. See: http://www.isthe.com/chongo/tech/comp/fnv/
proc hashString(str: ptr TString) =
  var hash = 2166136261u
  for i in 0..<str.length:
    hash = hash xor uint(str.value[i])
    hash *= 16777619

  str.hash = uint32(hash)

proc allocateString(gc: GC, length: int): ptr TString =
  result = gc.allocateFlex(TString, char, length + 1)
  gc.init(result.obj, okString)
  result.length = uint32(length)
  result.value[length] = char(0)

proc newString(gc: GC, text: cstring, length: int): Rooted[ObjString] =
  assert(length == 0 or text != nil, "Unexpected null string.")
  let str = gc.allocateString(length)
  if length > 0 and text != nil:
    copyMem(addr str.value[0], text, length)
  hashString(str)
  result = gc.root(cast[ObjString](str))

proc newString(gc: GC, text: cstring): Rooted[ObjString] = 
  gc.newString(text, text.len)

