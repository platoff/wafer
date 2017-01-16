
#import value
include vm

proc defineClass(vm: var WrenVM, module: ObjModule, name: cstring): ObjClass =
  let nameString = vm.newString(name)
  vm.pushRoot(nameString.asObj)
  result = vm.newSingleClass(0, nameString)
  discard vm.defineVariable(module, name, nameString.len, result)
  vm.popRoot(nameString.asObj)

proc initializeCore(vm: var WrenVM) =
  let coreModule = vm.newModule(nil)
  vm.pushRoot coreModule
  vm.mapSet(vm.modules, NullVal, coreModule)
  vm.popRoot coreModule

  template primitive(cls: ObjClass, name: cstring, code: untyped): untyped =    
    block:
      let symbol = vm.ensure(vm.methodNames, name, name.len)
      const fn = proc(wvm: var WrenVM, args: HugeArray[Value]): bool {.nimcall.} =
        template ret(v: untyped) =
          when v is Value:
            args[0] = v
          else:
            args[0] = v.asVal
          return true
        template err(msg: cstring) =
          wvm.fiber.error = wvm.newString(msg, msg.len)
          return false
        template arg(i): Value = 
          args[i]
        template wrenvm(): untyped = 
          wvm
        code
      var meth: Method
      meth.kind = METHOD_PRIMITIVE
      meth.primitive = fn
      vm.bindMethod(cls, symbol, meth)

  vm.objectClass = defineClass(vm, coreModule, "Object")
  vm.objectClass.primitive "!": ret false
  vm.objectClass.primitive "==(_)": ret(arg(0) == arg(1))
  vm.objectClass.primitive "!=(_)": ret(not (arg(0) == arg(1)))
  
  vm.objectClass.primitive "is(_)":
    if not isClass(arg(1)):
      err("Right operand must be a class.")
    var classObj = wrenvm.getClass(arg(0))
    let baseClass = vmcast[ObjClass](arg(1))
    # Walk the superclass chain looking for the class.
    while classObj != nil:
      if baseClass == classObj:
        ret true
      classObj = classObj.superclass
    ret false
  
  vm.objectClass.primitive "toString":
    let obj = arg(0).asObj
    let className = obj.classObj.name.c_value()
    ret wrenvm.stringFormat("instance of $", className)
  
  vm.objectClass.primitive "type":
    ret wrenvm.getClass(arg(0))

var
  wvm: WrenVM

init(wvm, 65536)
wvm.initializeCore()
