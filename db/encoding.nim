
import bytes
import endians

type
  E* = int
  A* = distinct int

  DBType* = E | A | int | bool | string

proc write*[N](b: var ByteArray[N], v: int) {.inline.} =
  var ini = v
  var outv: int
  when sizeof(int) == 8:
    bigEndian64(addr outv, addr ini)
  else:
    bigEndian32(addr outv, addr ini)
  
  b.add(initBytes(addr outv, sizeof(int)))

proc write*[N](b: var ByteArray[N], v: A) {.inline.} = write(b, int(v))

proc write*[N](b: var ByteArray[N], v: bool) {.inline.} = b.add byte(int(v))

proc write*[N](b: var ByteArray[N], v: string) {.inline.} = add(b, v.toBytes)

proc toBstring*(v: int): bstring =
  newSeq[byte](result, sizeof int)
  var ini = v
  var outv: int
  when sizeof(int) == 8:
    bigEndian64(addr outv, addr ini)
  else:
    bigEndian32(addr outv, addr ini)
  initBytes(addr outv, sizeof(int)).copyTo(addr result[0])

proc toBstring*(v: A): bstring = toBstring(int(v))

proc toBstring*(v: bool): bstring = @[byte(int(v))]

proc toBstring*(v: var string): bstring = 
  newSeq[byte](result, v.len)
  copyMem(addr result[0], addr v[0], v.len)
