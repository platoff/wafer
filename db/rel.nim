
import bytes
import encoding

const PageSize = 65536 * 8

type
  AccessKind = enum
    fixedStart
    fixedEnd
    variable

  Layout = object
    kind: AccessKind
    vbegin: int
    vend: int

  Rel* = ref object
    data: seq[bytes]
    ofs: int
    variables: int
    layout: array[16, Layout]
    sizes: seq[int]
    buffer: array[PageSize, byte]

proc binaryFind[T](v: openarray[T], key: T): int =
  var
    left:  int = 0
    right: int = v.len - 1

  while left <= right:
    let probe = (left + right) div 2
    let cmp = cmp(v[probe], key)
    if cmp < 0:
      left = probe + 1
    elif cmp > 0:
      right = probe - 1
    else:
      return probe
  
  result = -(left + 1)

proc sizes*(rel: Rel): seq[int] {.inline.} = rel.sizes

proc newRel*(sizes: openarray[int]): Rel =
  new result
  result.data = newSeq[bytes]()
  result.sizes = newSeq[int]()
  result.sizes.add sizes
  var i = 0
  var offset = 0
  while i < sizes.len and sizes[i] >= 0:
    result.layout[i].kind = fixedStart
    result.layout[i].vbegin = offset
    inc offset, sizes[i]
    result.layout[i].vend = offset - 1
    inc i
  let varsized = i
  i = sizes.len - 1
  var eoffset = 0
  while i > varsized:
    let s = sizes[i]
    assert s >= 0
    result.layout[i].kind = fixedEnd
    result.layout[i].vend = eoffset + 1
    inc eoffset, s
    result.layout[i].vbegin = eoffset
    dec i
  if varsized < sizes.len:
    result.layout[i].kind = variable
    result.layout[i].vbegin = offset
    result.layout[i].vend = eoffset + 1

  result.variables = sizes.len

proc clear*(rel: Rel) =
  rel.data.setLen(0)
  rel.ofs = 0

proc variables*(r: Rel): int {.inline.} = r.variables

proc put(rel: Rel, b: bytes) =
  let i = binaryFind(rel.data, b)
  if i < 0:
    assert(rel.ofs + b.len <= PageSize)
    let copy = addr rel.buffer[rel.ofs]
    b.copyTo(copy)
    inc rel.ofs, b.len
    let r = initBytes(copy, b.len)
    rel.data.insert r, -i - 1

proc addX*[X](r: Rel, a: X) =
  var buf: ByteArray[256]
  buf.write(a)
  r.put buf.toBytes

proc add*[X,Y](r: Rel, a: X, b: Y) =
  var buf: ByteArray[256]
  buf.write(a)
  buf.write(b)
  r.put buf.toBytes

proc add*[X,Y,Z](r: Rel, a: X, b: Y, c: Z) =
  var buf: ByteArray[256]
  buf.write(a)
  buf.write(b)
  buf.write(c)
  r.put buf.toBytes

proc add*(r: Rel, b: openarray[bytes]) =
  var buf: ByteArray[256]
  for i in b:
    buf.add(i)
  r.put buf.toBytes

proc showData*(rel: Rel) = 
  for i in rel.data:
    echo "data: ", i

#
# Iter
#

type
  TrieIter* = ref object
    rel: Rel
    pos: int
    depth: int
    atEnd: bool
    lengths: array[16, int]

proc newTrieIter*(rel: Rel): TrieIter =
  result = new TrieIter  
  result.rel = rel
  result.depth = -1
  result.atEnd = true

proc access(i: TrieIter, pos: int): bytes = 
  case i.rel.layout[i.depth].kind
  of fixedStart:
    i.rel.data[pos].substring(i.rel.layout[i.depth].vbegin, i.rel.layout[i.depth].vend)
  of fixedEnd:
    let len = i.rel.data[pos].len
    i.rel.data[pos].substring(len - i.rel.layout[i.depth].vbegin, len - i.rel.layout[i.depth].vend)
  of variable:
    let len = i.rel.data[pos].len
    i.rel.data[pos].substring(i.rel.layout[i.depth].vbegin, len - i.rel.layout[i.depth].vend)
  # echo "access: depth: ", i.depth, " pos: ", i.pos, " : ", a
  # result = a

proc next(i: TrieIter, start, len: int): int =
  result = start
  let skey = i.access(start)
  while true:
    inc result
    if result >= len:
      break
    if cmp(skey, i.access(result)) != 0:
      break

proc open*(i: TrieIter) =
  assert i.depth < i.rel.data.len, "can't open"
  if i.depth == -1:
    i.pos = 0
    i.lengths[0] = i.rel.data.len
    i.atEnd = i.rel.data.len == 0
  else:
    #echo "len/depth: ", i.lengths[i.depth], " pos: ", i.pos
    i.atEnd = i.pos >= i.lengths[i.depth]
    i.lengths[i.depth + 1] = i.next(i.pos, i.lengths[i.depth])
    #echo "len/depth + 1: ", i.lengths[i.depth + 1], " pos: ", i.pos
  inc i.depth

proc up*(i: TrieIter) =
  assert i.depth >= 0, "fully closed"
  if i.atEnd:
    dec i.pos
  dec i.depth
  if i.depth < 0:
    i.atEnd = true
  else:
    i.atEnd = i.pos >= i.lengths[i.depth]

proc atEnd*(i: TrieIter): bool {.inline.} = i.atEnd
proc key*(i: TrieIter): bytes {.inline.} = i.access(i.pos)
proc next*(i: TrieIter) {.inline.} = 
  i.pos = i.next(i.pos, i.lengths[i.depth])
  i.atEnd = i.pos >= i.lengths[i.depth]

proc seek*(i: TrieIter, key: bytes) =
  while true:
    if i.atEnd or i.key >= key:
      break 
    i.next

proc `$`*(i: TrieIter): string = "TrieIter"

when isMainModule:
  var r = newRel([sizeof int, -1, sizeof int])

  r.add(3, "hey", 42)
  r.add(1, "there", 55)
  r.add(1, "there", 77)
  r.add(0, "x", 55)
  r.add(1, "a", 88)
  r.add(7, "fuckit", 42)
  r.add(7, "aargh", 11)


  #echo repr r.layout

  var iter = r.newTrieIter()
  iter.open()
  while not iter.atEnd:
    echo iter.key
    #
    iter.open()
    while not iter.atEnd:
      echo " - ", iter.key
      #
      iter.open()
      while not iter.atEnd:
        echo " -- ", iter.key
        #
        #
        iter.next
      iter.up()
      #
      iter.next
    iter.up()
    #
    iter.next()
  iter.up()
