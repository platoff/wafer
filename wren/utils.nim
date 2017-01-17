

type
  SizeType = uint16
  FlexibleArray*{.unchecked.}[T] = array[0..0, T]
  PArray*[T] = ptr FlexibleArray[T]

  Buffer*[T] = object
    data: PArray[T]
    count: int

  # String = object
  #   buffer*: cstring
  #   length*: int

  # SymbolTable* = Buffer[String]

const
  CountBits = 24
  CapacityOne = 1 shl CountBits
  CountMask = CapacityOne - 1

import strutils

proc grow[T](buf: var Buffer[T], currentCap: int) =
  var capacity: int
  if currentCap == 0:
    capacity = 4
    buf.count = 3 shl CountBits
  else:
    capacity = currentCap shl 1
    inc buf.count, CapacityOne
  #echo "realloc to ", capacity  

template `[]`*[T](buf: Buffer[T], i: int): T = buf.data[i]
template `[]=`*[T](buf: var Buffer[T], i: int, val: T) = buf.data[i] = val
template len*[T](buf: Buffer[T]): int = (buf.count and CountMask)

proc add*[T](buf: var Buffer[T], val: T) =
  let capacity = (1 shl (buf.count shr CountBits)) shr 1
  if buf.len == capacity:
    grow(buf, capacity)
  inc buf.count

var b: Buffer[int]

for i in 0..222:
  b.add i

b[0] = 77

for i in 1..222:
  b[i] = b[i-1]

