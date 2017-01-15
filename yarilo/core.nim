
# import value
# import gc
# import vm

include vm

#template PRIMITIVE() =

#define DEF_PRIMITIVE(name) \
#    static bool prim_##name(WrenVM* vm, Value* args)

#define RETURN_VAL(value)   do { args[0] = value; return true; } while (0)

#define RETURN_OBJ(obj)     RETURN_VAL(OBJ_VAL(obj))
#define RETURN_BOOL(value)  RETURN_VAL(BOOL_VAL(value))
#define RETURN_FALSE        RETURN_VAL(FALSE_VAL)
#define RETURN_NULL         RETURN_VAL(NULL_VAL)
#define RETURN_NUM(value)   RETURN_VAL(NUM_VAL(value))
#define RETURN_TRUE         RETURN_VAL(TRUE_VAL)

proc defineClass(vm: VM, module: ObjModule, name: cstring): ObjClass =
  let nameString = stringFormat(vm.gc, "$", name)
  pushRoot(vm.gc, nameString)
  result = newSingleClass(vm.gc, 0, nameString)
  discard vm.gc.defineVariable(module, name, nameString.len, result.asVal)
  popRoot(vm.gc, nameString)

proc initializeCore(vm: VM) =
  let gc = vm.gc
  let system = gc.newString("system")
  gc.pushRoot system
  let coreModule = gc.newModule(system)
  gc.pushRoot coreModule
  gc.mapSet(vm.modules, system.asVal, coreModule.asVal)
  gc.popRoot coreModule
  gc.popRoot system

  template primitive(cls: ObjClass, name: cstring, code: untyped): untyped =    
    block:
      let symbol = ensure(gc, vm.methodNames, name, name.len)
      let fn = proc(wvm: VM, args: HugeArray[Value]): bool {.nimcall.} =
        template ret(v: untyped) =
          when v is Value:
            args[0] = v
          else:
            args[0] = v.asVal
          return true
        template err(msg: cstring) =
          wvm.fiber.error = gc.newString(msg, msg.len).asVal
          return false
        template arg(i): Value = 
          args[i]
        code
      var meth: Method
      meth.kind = METHOD_PRIMITIVE
      meth.primitive = cast[Primitive](fn)
      bindMethod(gc, cls, symbol, meth)

  let objectClass = defineClass(vm, coreModule, "Object")
  gc.classes[okObject] = objectClass
  objectClass.primitive "!": ret false
  objectClass.primitive "==(_)": ret(arg(0) == arg(1))
  objectClass.primitive "!=(_)": ret(not (arg(0) == arg(1)))
  objectClass.primitive "is(_)":
    if not isClass(arg(1)):
      err("Right operand must be a class.")



  PRIMITIVE(vm->objectClass, "is(_)", object_is);
  PRIMITIVE(vm->objectClass, "toString", object_toString);
  PRIMITIVE(vm->objectClass, "type", object_type);


DEF_PRIMITIVE(object_is)
{
  if (!IS_CLASS(args[1]))
  {
    RETURN_ERROR("Right operand must be a class.");
  }

  ObjClass *classObj = wrenGetClass(vm, args[0]);
  ObjClass *baseClassObj = AS_CLASS(args[1]);

  // Walk the superclass chain looking for the class.
  do
  {
    if (baseClassObj == classObj) RETURN_BOOL(true);

    classObj = classObj->superclass;
  }
  while (classObj != NULL);

  RETURN_BOOL(false);
}

DEF_PRIMITIVE(object_toString)
{
  Obj* obj = AS_OBJ(args[0]);
  Value name = OBJ_VAL(obj->classObj->name);
  RETURN_VAL(wrenStringFormat(vm, "instance of @", name));
}

DEF_PRIMITIVE(object_type)
{
  RETURN_OBJ(wrenGetClass(vm, args[0]));
}


var
  gcobj: GCObj
init(gcobj, 65536)
let wvm = newVM(addr gcobj)
wvm.initializeCore()
