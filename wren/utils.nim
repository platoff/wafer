import macros

proc getName(node: NimNode): string =
  case node.kind
  of nnkPostfix:
    return $node[1].ident
  of nnkIdent:
    return $node.ident
  # of nnkEmpty:
  #   return "anonymous"
  else:
    echo $node.kind
    error("Unknown name: " & node.treeRepr)

proc transformApiProc(fn: NimNode): NimNode =
  let name = fn[0].getName
  hint("Processing '" & name & "' as a WrenVM proc.")
  let prc = fn.copyNimTree
  let params = prc[3]  # Formal Parameters
  
  params.insert(1, newIdentDefs(ident("vm"), ident("VM")))

  # # First param must be a VM reference.
  # let paramType = params[1][^2]
  # if paramType.ident != !"VM":
  #   error("WrenVM proc must accept VM as first parameter.")

  let tmplParams = newNimNode nnkFormalParams
  tmplParams.add params[0]

  let call = newNimNode nnkCall
  call.add newDotExpr(newIdentNode("vm"), newIdentNode(name))

  for p in 2..<params.len:
    #echo "template param: ", params[p].treeRepr
    tmplParams.add params[p].copyNimTree
    for i in 0..<(params[p].len - 2):
      call.add params[p][i].copyNimNode

  let tmpl = newNimNode(nnkTemplateDef)
  tmpl.add fn[0].copyNimTree
  tmpl.add fn[1].copyNimTree
  tmpl.add fn[2].copyNimTree
  tmpl.add tmplParams
  tmpl.add newEmptyNode()
  tmpl.add newEmptyNode()
  tmpl.add newStmtList(
    newNimNode(nnkMixinStmt).add(newIdentNode("vm")),
    call
  )
    
  result = newStmtList()
  result.add prc
  result.add tmpl
  #echo result.treeRepr


macro wren*(fn: untyped): untyped =
  result = transformApiProc(fn)
