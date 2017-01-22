

type 
  Obj = object {.inheritable.}

  OInt = object of Obj
    i: int
  OString = object of Obj
    s: cstring
  OWord = object of Obj
    a: cstring
    b: int

method x(o: ptr Obj): int {.base.} = discard
method x(o: ptr OInt): int = 
  o.i * 2
method x(o: ptr OString): int = 
  o.s.len
method x(o: ptr OWord): int = 
  o.a.len + o.b

proc show(x: ptr Obj) =
  echo x.x()

var 
  a: OInt
  b: OString 
  c: OWord

a.i = 5
show addr a
b.s = "hey there"
show addr b
c.a = "OK Bro"
c.b = 33
show addr c

echo int(false)
echo int(true)
