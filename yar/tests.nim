
import unittest

include values

suite "VM":
  let vm = newVM()
  let root = cast[Context](vm.context)

  test "Constants":
    let a = vm.newString("hey there")
    let b = vm.newString("hey there")
    check a == b
    check a.pointer == b.pointer
    let c = vm.newString("hey there!!")
    check a != c
    check a.pointer != c.pointer

  test "Conversions":
    let a = vm.newString("hey there")
    let w = vm.newWord(a, wkWord)
    let val = value(w)
    check((cast[int](w) or tagConst.int) == cast[int](val))

  test "Parse":
    let blk = vm.parse("i: 7")
    let v = blk.data[1]
    check v.isNum
    check v.asNum == 7

  test "Lookup":
    let a = vm.newString("hey there")
    let ip = vm.lookup(root, a, true)
    check ip != nil
    check(ip[].int == Null.int)
    let ip2 = vm.lookup(root, a, false)
    check ip2 != nil
    check(ip == ip2)

  test "Interpret":
    let blk = vm.parse("i: 7")
    discard vm.interpret(addr blk.data[0], blk.data[0])
    check vm.stack.count == 1
    check vm.stack[0].int == value(7).int
    vm.drop
    let i = vm.newString("i")
    let iword = vm.lookup(root, i, false)
    check iword != nil
    check iword[].int == value(7).int

  test "Interpret Func":
    let blk = vm.parse("func [] []")
    discard vm.interpret(addr blk.data[0], blk.data[0])
    vm.showStack()
    vm.drop

  test "Interpret Block":
    let blk = vm.parse("print 777 print 888 print add 1 2 print lt 5 8 print lt 5 3")
    vm.push Null
    vm.interpretBlock(blk)
    vm.showStack()
    vm.drop

  test "Interpret While":
    let blk = vm.parse("j: 0 while [lt j 10] [print j j: add j 1]")
    vm.push Null
    vm.interpretBlock(blk)
    vm.showStack()
    vm.drop

  test "Interpret Vector":
    let blk = vm.parse("make-vector! [1 2 3]")
    vm.push Null
    vm.interpretBlock(blk)
    vm.showStack()
    vm.drop

  test "Interpret Func Call":
    let blk = vm.parse("fn: func [a b] [print add a b] fn 7 100")
    vm.push Null
    vm.interpretBlock(blk)
    vm.showStack()
    vm.drop

  test "Interpret Reactive":
    let blk = vm.parse("v1: make-vector! [1 2 3] v2: react [v1] print v1 print v2")
    vm.push Null
    vm.interpretBlock(blk)
    vm.showStack()
    vm.drop
