
import model
import core

import unittest

suite "Data structures":
  let gc = create WrenVM
  init(gc, 65536)

  test "conversions":
    #check val(nil) == NullVal
    discard

  test "map set/get":
    let map = gc.newMap()
    gc.pushRoot map
    let str = gc.newString("Hello, World")
    gc.pushRoot str

    gc.mapSet(map, NullVal, str.val)
    gc.popRoot str
    
    let val = map[NullVal]
    check isObj(val)

    let obj = val.asObj
    echo repr cast[ObjString](obj)
#     check obj.kind == okString
    
#     let objString = val.asString
#     check objString.value == "Hello, World"
#     echo objString.value
    gc.popRoot map

suite "VM":
  let wvm = create WrenVM
  wvm.init(16 * 1024 * 1024)
  wvm.initializeCore()

#   test "2 + 2":
#     check wvm.interpret("""

# var yMin = 1
# var yMax = 2
# var xMin = 3
# var xMax = 4

# System.print(xMax)


#     """) == CompileError
