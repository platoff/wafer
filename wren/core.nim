
import model
import vm

proc defineClass(vm: VM, module: ObjModule, name: cstring): ObjClass =
  let nameString = vm.newString(name)
  vm.pushRoot(nameString)
  result = vm.newSingleClass(0, nameString)
  discard vm.defineVariable(module, name, nameString.len, result.val)
  vm.popRoot(nameString)

proc findVariable(vm: VM, module: ObjModule, name: cstring): Value =
  let symbol = find(module.variableNames, name, len(name))
  result = module.variables[symbol]


proc initializeCore*(vm: VM) =
  let coreModule = vm.newModule(nil)
  vm.pushRoot coreModule
  vm.mapSet(vm.modules, NullVal, coreModule.val)
  vm.popRoot coreModule

  template primitive(cls: ObjClass, name: cstring, code: untyped): untyped =    
    block:
      let symbol = vm.ensure(vm.methodNames, name, name.len)
      const fn = proc(wvm: VM, args: PArray[Value]): bool {.nimcall.} =
        template ret(v: untyped) =
          when v is Value:
            args[0] = v
          else:
            args[0] = v.val
          return true
        template err(msg: cstring) =
          wvm.fiber.error = wvm.newString(msg, msg.len).val
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

  # Now we can define Class, which is a subclass of Object.
  vm.classClass = defineClass(vm, coreModule, "Class")
  bindSuperclass(vm, vm.classClass, vm.objectClass)

  vm.classClass.primitive "name":
    ret vmcast[ObjClass](arg(0)).name

  vm.classClass.primitive "supertype":
    let objClass = vmcast[ObjClass](arg(0))
    ret if objClass.superclass == nil: NullVal else: objClass.superclass.val

  vm.classClass.primitive "toString":
    ret vmcast[ObjClass](arg(0)).name

  # Finally, we can define Object's metaclass which is a subclass of Class.
  let objectMetaclass = vm.defineClass(coreModule, "Object metaclass")

  # Wire up the metaclass relationships now that all three classes are built.
  vm.objectClass.obj.classObj = objectMetaclass
  objectMetaclass.obj.classObj = vm.classClass
  vm.classClass.obj.classObj = vm.classClass

  # Do this after wiring up the metaclasses so objectMetaclass doesn't get
  # collected.
  bindSuperclass(vm, objectMetaclass, vm.classClass)

  objectMetaclass.primitive "same(_,_)":
    ret arg(1) == arg(2)

  # The core class diagram ends up looking like this, where single lines point
  # to a class's superclass, and double lines point to its metaclass:
  
  #        .------------------------------------. .====.
  #        |                  .---------------. | #    #
  #        v                  |               v | v    #
  #   .---------.   .-------------------.   .-------.  #
  #   | Object  |==>| Object metaclass  |==>| Class |=="
  #   '---------'   '-------------------'   '-------'
  #        ^                                 ^ ^ ^ ^
  #        |                  .--------------' # | #
  #        |                  |                # | #
  #   .---------.   .-------------------.      # | # -.
  #   |  Base   |==>|  Base metaclass   |======" | #  |
  #   '---------'   '-------------------'        | #  |
  #        ^                                     | #  |
  #        |                  .------------------' #  | Example classes
  #        |                  |                    #  |
  #   .---------.   .-------------------.          #  |
  #   | Derived |==>| Derived metaclass |=========="  |
  #   '---------'   '-------------------'            -'

  # The rest of the classes can now be defined normally.
  const coreModuleSource = cstring(staticRead"core.wren")
  echo interpretInModule(vm, nil, coreModuleSource)

  vm.numClass = vmcast[ObjClass](findVariable(vm, coreModule, "Num"))
  vm.numClass.primitive "toString":
    ret wrenvm.newString(($arg(0).asNum).cstring).val


  vm.stringClass = vmcast[ObjClass](findVariable(vm, coreModule, "String"))
  vm.stringClass.primitive "toString":
    ret arg(0)
  
  let systemClass = vmcast[ObjClass](findVariable(vm, coreModule, "System"))
  systemClass.obj.classObj.primitive  "writeString_(_)":
    echo "* print: ", vmcast[ObjString](arg(1)).c_value
    ret arg(1)

  # While bootstrapping the core types and running the core module, a number
  # of string objects have been created, many of which were instantiated
  # before stringClass was stored in the VM. Some of them *must* be created
  # first -- the ObjClass for string itself has a reference to the ObjString
  # for its name.
  
  # These all currently have a NULL classObj pointer, so go back and assign
  # them now that the string class is known.
  var obj = vm.first
  while obj != nil:
    if obj.kind == okString:
      echo "string class: ", cast[ObjString](obj).c_value
      obj.classObj = vm.stringClass
    obj = obj.next
