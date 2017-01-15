

import value
import gc
import vm

import unittest

suite "Data structures":
  var gcObj: GCObj
  init(gcObj, 65536)
  let gc = cast[GC](addr gcObj)

  test "map set/get":
    let map = gc.newMap()
    gc.pushRoot map
    let str = gc.newString("Hello, World")
    gc.pushRoot str

    gc.mapSet(map, NullVal, str.asVal)
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
  var gcObj: GCObj
  init(gcObj, 65536)
  let gc = addr gcObj
  let vm = newVM(gc)

  test "load module":
    vm.loadModule(gc.newString("Hello, World").asVal, "xxx")
