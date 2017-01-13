
type
  FlexibleArray{.unchecked.}[T] = array[0..0, T]
  HugeArray*[T] = ptr FlexibleArray[T]

  Buffer*[T] = object
    data: HugeArray[T]
    count: uint32
    capacity: uint32

  String = object
    buffer: cstring
    length: int

  SymbolTable* = Buffer[String]

proc buffer*(s: String): cstring = s.buffer
proc length*(s: String): int = s.length

#
# Allocator helpers
#

proc deallocate*[A](a: A, p: pointer) = 
  discard a.reallocate(p, 0, 0)

proc allocate*[A](a: A, T: typedesc): ptr T = 
  result = cast[ptr T](a.reallocate(nil, 0, sizeof T))
  zeroMem(result, sizeof T)

proc allocate*[A](a: A, T: typedesc, n: int): HugeArray[T] = 
  cast[HugeArray[T]](a.reallocate(nil, 0, n * sizeof T))

proc allocateFlex*[A](a: A, T: typedesc, I: typedesc, n: int): ptr T = 
  cast[ptr T](a.reallocate(nil, 0, sizeof(T) + n * sizeof I))

# proc init*(buffer: var Buffer) =
#   buffer.data = nil
#   buffer.count = 0
#   buffer.capacity = 0  

proc len*(buffer: Buffer): int = int(buffer.count) 

iterator items*[T](buf: Buffer[T]): T =
  for i in 0..<int(buf.count):
    yield buf.data[i]

proc `[]`*[T](buf: Buffer[T], i: int): T = 
  assert i < buf.len
  buf.data[i]
  
proc `[]=`*[T](buf: var Buffer[T], i: int, val: T) = 
  assert i < buf.len
  buf.data[i] = val

# From: http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2Float
proc powerOf2Ceil(n: int): int =
  result = n - 1
  result = result or (n shr 1)
  result = result or (n shr 2)
  result = result or (n shr 4)
  result = result or (n shr 8)
  result = result or (n shr 16)
  result = result + 1

proc fill[A,T](allocator: A, buffer: var Buffer[T], value: T, count: int) =
  let newCount = int(buffer.count) + count
  if int(buffer.capacity) < newCount:
    let capacity = powerOf2Ceil(newCount)
    buffer.data = 
      cast[HugeArray[T]](allocator.reallocate(buffer.data, int(buffer.capacity) * sizeof(T), capacity * sizeof(T)))
    for i in 0..<count:
      buffer.data[buffer.count] = value
      inc buffer.count

proc add*[A,T](allocator: A, buffer: var Buffer[T], val: T) = allocator.fill(buffer, val, 1)

#
# Symbol Table
# 

proc find*(table: SymbolTable, name: cstring, length: int): int =
  for i in 0..<int(table.count):
    if table.data[i].length == length and
      equalMem(table.data[i].buffer, name, length):
        return i
  result = -1

proc add*[A](allocator: A, symbols: var SymbolTable, name: cstring, length: int): int =
  var symbol: String
  symbol.buffer = cast[cstring](allocator.allocate(char, length + 1))
  copyMem(symbol.buffer, name, length)
  symbol.buffer[length] = char(0)
  symbol.length = (int)length
  allocator.add(symbols, symbol)
  result = int(symbols.count) - 1

