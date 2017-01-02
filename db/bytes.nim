
import strutils

type
  bytes* = object
    data: pointer
    len: int

  ByteArray*[N: static[int]] = object
    len: int
    buf: array[N, byte]

proc initBytes*(p: pointer, ofs, len: int): bytes {.inline, noSideEffect.} =
  result.data = cast[pointer](cast[ByteAddress](p) +% ofs)
  result.len = len

proc initBytes*(p: pointer, len: int): bytes {.inline, noSideEffect.} = 
  initBytes(p, 0, len)

proc nthAddress(b: bytes, n: int): ByteAddress {.inline, noSideEffect.} =
  assert((n <= b.len) and (n >= 0))
  result = cast[ByteAddress](b.data) +% n

proc firstAddress(b: bytes): ByteAddress {.inline, noSideEffect.} =
  b.nthAddress(0)

proc nth*(b: bytes, n: int): ptr byte {.inline, noSideEffect.} =
  cast[ptr byte](nthAddress(b, n))

proc `[]`*(b: bytes, i: int): int {.inline, noSideEffect.} = int(b.nth(i)[])

proc `[]=`*(b: bytes, i: int, v: int) {.inline.} = b.nth(i)[] = byte(v)

proc len*(b: bytes): int {.inline, noSideEffect.} = b.len

proc substring*(b: bytes, start, finish: int): bytes {.inline, noSideEffect.} =
  let sublen = finish - start + 1
  assert(sublen >= 0)
  assert((start >= 0) and (start <= b.len))
  assert((finish >= -1) and (finish < b.len))
  result = bytes(data: cast[pointer](cast[ByteAddress](b.data) +% start), len: sublen)

proc substring*(b: bytes, start: int): bytes {.inline, noSideEffect.} =
  substring(b, start, b.len-1)

proc head*(b: bytes, n: int): bytes {.inline, noSideEffect.} = 
  substring(b, 0, n-1)

proc tail*(b: bytes, n: int): bytes {.inline, noSideEffect.} = 
  substring(b, b.len-n, b.len-1)

proc copyTo*(b: bytes, p: pointer) {.inline.} = 
  copyMem(p, b.data, b.len)

proc findCommonPrefixLen*(this, that: bytes): int =
  var remaining = min(this.len, that.len)
  var pthis: ByteAddress = this.firstAddress
  var pthat: ByteAddress = that.firstAddress

  while (remaining != 0) and 
        (cast[ptr byte](pthis)[] == cast[ptr byte](pthat)[]):
    inc(pthis)
    inc(pthat)
    dec(remaining)

  result = pthis - this.firstAddress

iterator items*(b: bytes): int {.inline, noSideEffect.} =
  var a = cast[ByteAddress](b.data)
  for _ in 0..<b.len:
    yield int(cast[ptr byte](a)[])
    inc(a)

proc cmp*(a, b: bytes): int {.noSideEffect.} =
  var remaining = min(a.len, b.len)
  var pa: ByteAddress = a.firstAddress
  var pb: ByteAddress = b.firstAddress

  while remaining != 0:
    let va = cast[ptr byte](pa)[]
    let vb = cast[ptr byte](pb)[]
    if va < vb: return -1
    if va > vb: return 1
    inc(pa)
    inc(pb)
    dec(remaining)

  result = a.len - b.len

proc `<=`*(a, b: bytes): bool {.inline, noSideEffect.} = cmp(a, b) <= 0
proc `==`*(a, b: bytes): bool {.inline, noSideEffect.} = cmp(a, b) == 0

proc `$`*(b: bytes): string =
  result = "["
  for i in b:
    result.add(toHex(i, 2))
    result.add(" ")
  for i in b:
    if i in 0x20..0x7E:
      result.add(chr(i))
    else:
      result.add(".")
  result.add("]")

converter toBytes*(s: string): bytes {.inline, noSideEffect.} = 
  result.data = cstring(s)
  result.len = len(s)

proc padTail*[N](b: var ByteArray[N], len: int) {.inline.} = 
  assert(sizeof(b.buf) - b.len >= len) # check if we have enough space
  inc(b.len, len)

proc add*[N](b: var ByteArray[N], v: byte) {.inline.} =
  let len = b.len
  b.padTail(1)
  b.buf[len] = v

proc add*[N](b: var ByteArray[N], v: bytes) {.inline.} =
  let len = b.len
  b.padTail(v.len)
  v.copyTo(addr b.buf[len])
  
proc len*[N](b: ByteArray[N]): int {.inline, noSideEffect.} = b.len

proc `[]=`*[N](b: var ByteArray[N], i: int, v: int) {.inline.} =
  assert(i < b.len and i >= 0) 
  b.buf[i] = (byte)v

proc `[]`*[N](b: ByteArray[N], i: int): int {.inline, noSideEffect.} =
  assert(i < b.len and i >= 0) 
  b.buf[i]

converter toBytes*[N](b: var ByteArray[N]): bytes {.inline, noSideEffect.} = 
  result.data = addr(b.buf)
  result.len = b.len

type
  bstring* = seq[byte]

proc newBString*(len: int): bstring = newSeq[byte](result, len)
