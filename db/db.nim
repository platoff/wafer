

import bytes
import rel
import encoding
import tables
import algorithm

type
  DB = ref object
    eav: Rel
    ave: Rel

  TermKind = enum
    tkVar
    tkConst

  Term = object
    case kind: TermKind
    of tkVar: name: string
    of tkConst: val: bstring

const 
  MaxVars = 16
  MaxPreds = 16

type
  Matrix = array[MaxPreds, array[MaxVars, int]]


type
  Pred = ref object
    db: DB
    terms: seq[Term]

  Var = object
    name: string
    select: bool
    
  Query = ref object
    vars: seq[Var]
    preds: seq[Pred]
    m: Matrix

proc newDB(): DB = 
  new result
  result.eav = newRel([sizeof E, sizeof A, -1])
  result.ave = newRel([sizeof A, -1, sizeof E])  

proc add[V](db: DB, e: E, a: A, v: V) =
  db.eav.add(e, a, v)
  db.ave.add(a, v, e)

proc `$`(t: Term): string =
  case t.kind
  of tkVar: t.name
  of tkConst: $t.val

proc `$`(p: Pred): string = $p.terms

# Estimate if it's possible to build an iterator for
# specified predicate in specified order.$
# returns 0 if no iterator can be provider, or value of iterator --
# bigger values mean better iterator (executes faster and produces less results)
proc estimateIter(pred: Pred, order: array[MaxVars, int]): int =
  #echo "requesing iterator for ", pred, " in the following order ", @order
  #result = 1
  for i in order:
    if i != 0:
      return i

#
#
#

proc variable(name: string): Term =
  result.kind = tkVar
  result.name = name

proc constant(v: DBType): Term =
  result.kind = tkConst
  result.val = toBstring(v)

proc findOrAdd(q: Query, name: string): int =
  result = 0
  for i in q.vars:
    if i.name == name:
      return result
    inc result
  q.vars.setLen(result + 1)
  q.vars[result].name = name

proc pred(q: Query; db: DB, pred: varargs[Term]) = 
  var p = new Pred
  p.db = db
  p.terms = newSeq[Term]()
  p.terms.add pred

  for i, t in pred:
    if t.kind == tkVar:
      q.m[q.preds.len][q.findOrAdd(t.name)] = i + 1
  
  q.preds.add p

proc select(q: Query, vars: varargs[string]) =
  for v in vars:
    let i = q.findOrAdd(v)
    q.vars[i].select = true

type
  Plan = ref object
    score: int
    iorder: Matrix # list of all iterators with variable order [pred, var]
    execution: Matrix # seq[seq[int]] # execution plan, levels -> iterators

#
# Build query plan
# 

# proc newPlan(): Plan =
#   new result
#   result.execution = newSeq[seq[int]]()

proc showMatrix(m: Matrix, cols, rows: int) =
  for p in 0..< cols:
    var s = ""
    for v in 0..< rows:
      s.add $m[p][v]
      s.add " "
    echo s

proc generatePlan(q: Query, order: openarray[int]): Plan =
  new result

  # build iterators
  # var iters = newSeq[TrieIter]()
  #result.iorder = newSeq[seq[int]]()

  for i, p in q.preds:
    var j = 0
    for o in order:
      if q.m[i][o] > 0:
        result.iorder[i][j] = q.m[i][o] # - 1
        inc j

#    if iterOrder.len > 0:
    # iters.add iter(p, iterOrder)
    # orders.add iterOrder
#    else:
#      iters.add nil

  # build execution
  # result.execution = newSeq[seq[int]]()

  for j, o in order:
    # result.execution[j] = newSeq[int]()
    var levelscore = 0
    if q.vars[o].select:
      echo "emit ", q.vars[o].name
    for i in 0..< q.preds.len:
      if q.m[i][o] > 0:
        let estimation = estimateIter(q.preds[i], result.iorder[i])
        echo " -- iter ", i, " value: ", estimation
        inc levelscore, estimation
        result.execution[j][i] = estimation #.add i # iters[i]
    #plan.plan.add level
    echo "next depth"
    result.score = result.score * 16 + levelscore

  #showMatrix(result.iorder, q.preds.len, order.len)
  showMatrix(result.execution, order.len, q.preds.len)
  echo "SCORE: ", result.score



proc deletePred(q: Query, p: int) =
  echo "WARNING: unused predicate: ", q.preds[p]
  #q.showMatrix
  # for i in 0..< MaxVars:
  #   q.m[i, p] = 0
  #q.showMatrix
  q.preds.delete p
  for i in p..<(MaxPreds-1):
    q.m[i] = q.m[i+1]

proc prepare(q: Query) =

  echo "vars: ", q.vars
  showMatrix(q.m, q.preds.len, q.vars.len)

  var join = newSeq[int]()
  for v in 0..<q.vars.len:
    if q.vars[v].select:
      join.add v
    else:
      var pcount = 0
      for p in 0..<q.preds.len:
        if q.m[p][v] > 0: inc pcount
      if pcount > 1:
        join.add v
      else:
        # this is not a join var  - remove all terms having it if not select var
        # if not v.select:
        var k = 0
        while k < q.preds.len:
          if q.m[k][v] != 0:
            q.deletePred(k)
          else:
            inc k

  showMatrix(q.m, q.preds.len, q.vars.len)

  echo "join on vars: ", join
  sort(join, cmp[int])

  while true:
    echo join
    let plan = q.generatePlan(join)  
    if not nextPermutation(join):
      break


proc newQuery(): Query =
  new result
  result.preds = newSeq[Pred]()
  result.vars = newSeq[Var]()

when isMainModule:
  const Title = A(1)
  const Completed = A(2)

  var db = newDB()
  db.add(1, Title, "sun!")

  db.eav.showData()
  db.ave.showData()

  var q = newQuery()

  q.pred(db, variable("?e"), constant(Title), variable("?title"))
  q.pred(db, variable("?e"), constant(Completed), variable("?completed"))
  q.pred(db, variable("?completed"))
  q.pred(db, variable("?title"))
  q.select("?title", "?completed")
  q.prepare
