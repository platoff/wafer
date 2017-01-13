
include map

#
# Module
#

# The maximum number of module-level variables that may be defined at one time.
# This limitation comes from the 16 bits used for the arguments to
# `CODE_LOAD_MODULE_VAR` and `CODE_STORE_MODULE_VAR`.
const 
  MaxModuleVars = 65536

proc newModule*(gc: GC, name: ObjString): Rooted[ObjModule]  =
  let module = gc.allocate(TModule)
  module.name = name
  result = gc.root(cast[ObjModule](module))

proc defineVariable(gc: GC, module: ObjModule, name: cstring,
                       length: int, value: Value): int =
  if module.variables.len == MaxModuleVars: 
    return -2

  # See if the variable is already explicitly or implicitly declared.
  result = module.variableNames.find(name, length)
  if result == -1:
    # Brand new variable.
    result = gc.add(module.variableNames, name, length)
    gc.add(module.variables, value)
  elif isNum(module.variables[result]):
    ## An implicitly declared variable's value will always be a number. Now we
    ## have a real definition.
    module.variables[result] = value
  else:
    # Already explicitly declared.
    result = -1


