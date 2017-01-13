

include vm

import unittest

suite "smoke VM tests":
  let vm = newVM(65536)
  let gc = vm.gc

  test "test map set/get":
    let map = gc.newMap()
    let str = gc.newString("Hello, World")

    gc.mapSet(map[], NullVal, str[].asVal)
    let val = map[][NullVal]

    check val.kind == vkObj
    let obj = val.asObj
    check obj.kind == okString
    let objString = val.asString
    check objString.value == "Hello, World"
    echo objString.value


