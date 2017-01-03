

import bytes
import rel
import encoding
import tables
import algorithm
import strutils

const
  OrderLen = sizeof(uint) * 2
  MaxVars = OrderLen
  MaxPreds = 16
  MaxIndices = 4

type
  # I'm going to use 4 bits per number, so
  # 0x123 means [1,2,3], and so on
  Order = distinct uint

  # database is `an idexed rel`, which means is keep rel in various orders
  DB = ref object
    indices: int
    index: array[MaxIndices, Rel]
    orders: array[MaxIndices, Order]

  TermKind = enum
    tkVar
    tkConst

  Term = object
    case kind: TermKind
    of tkVar: name: string
    of tkConst: val: bstring

type
  Matrix = array[MaxPreds, array[MaxVars, int]]

  Pred = ref object
    db: DB
    terms: seq[Term]

  Var = object
    name: string
    select: bool
    
  Plan = ref object
    score: int
    iorder: array[MaxPreds, Order]
    execution: Matrix # seq[seq[int]] # execution plan, levels -> iterators
    
  Query = ref object
    vars: seq[Var]
    preds: seq[Pred]
    plan: Plan
    m: Matrix

proc add(order: var Order, i: int) {.inline.} =
  assert((i > 0) and (i < 16))
  order = Order((uint(order) shl 4) or uint(i))

proc `[]`(order: Order, i: int): int {.inline.} =
  int((uint(order) shr uint(i * 4)) and 0xf)

proc reorder[T](a: openarray[T], o: Order): seq[T] =
  newSeq[T](result, a.len)
  var i = a.len - 1
  var order = int(o)
  while i >= 0:
    result[i] = a[(order and 0xf)-1]
    order = order shr 4
    dec i

proc newDB(layout: openarray[int], indices: openarray[Order]): DB = 
  new result
  result.indices = indices.len
  for i, o in indices:
    result.orders[i] = o
    result.index[i] = newRel(layout.reorder(o))

proc add[V](db: DB, e: E, a: A, v: V) =
  var ebuf: ByteArray[sizeof E]
  var abuf: ByteArray[sizeof A]
  var vbuf: ByteArray[256]

  ebuf.write(e)
  abuf.write(a)
  vbuf.write(v)

  let b = [ebuf.toBytes, abuf.toBytes, vbuf.toBytes]

  for i in 0..<db.indices:
    db.index[i].add(b.reorder(db.orders[i]))

proc `$`(t: Term): string =
  case t.kind
  of tkVar: t.name
  of tkConst: $t.val

proc `$`(p: Pred): string = $p.terms

#proc `==`(a, b: Order): bool {.borrow.}
proc endWith(a, b: Order): bool =
  ((int(a) xor int(b)) and int(b)) == 0

# Estimate if it's possible to build an iterator for
# specified predicate in specified order.$
# returns 0 if no iterator can be provider, or value of iterator --
# bigger values mean better iterator (executes faster and produces less results)
proc estimateIter(pred: Pred, order: Order): int =
  #echo "estimating iterator: ", toHex(int(order))
  for i in 0..<pred.db.indices:
    if pred.db.orders[i].endWith(order):
      echo "build iterator use rel of the order: ", toHex(int(pred.db.orders[i]))
      return 1
  echo "!!!Can't build iterator for order ", toHex(int(order))

# proc estimateIter(pred: Pred, order: array[MaxVars, int]): int =
#   for i in order:
#     if i != 0:
#       return i

proc buildIter(pred: Pred, order: Order): TrieIter =
  echo "building iter for ", pred, " in order ", toHex(int(order))
  var rel: Rel
  var constants: uint
  for i in 0..<pred.db.indices:
    if pred.db.orders[i].endWith(order):
      echo "using rel of the order: ", toHex(int(pred.db.orders[i]))
      rel = pred.db.index[i]
      constants = uint(pred.db.orders[i]) xor uint(order)


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

#
# Build query plan
# 

# proc newPlan(): Plan =
#   new result
#   result.execution = newSeq[seq[int]]()

proc showMatrix(m: array[MaxPreds, Order], cols: int) =
  for p in 0..< cols:
    echo toHex(int(m[p]))

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
    for o in order:
      if q.m[i][o] > 0:
        result.iorder[i].add(q.m[i][o])
        #inc j

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

  #showMatrix(q.m, q.preds.len, q.vars.len)

  echo "join on vars: ", join
  sort(join, cmp[int])

  var bestPlanScore = 0
  while true:
    echo join
    let plan = q.generatePlan(join)
    if plan.score > bestPlanScore:
      q.plan = plan
      bestPlanScore = plan.score
    if not nextPermutation(join):
      break

proc execute(q: Query) =
  # build iterators - we'll build them according to var ordering in plan.iorder
  echo "vars: ", q.vars
  echo "preds: ", q.preds
  #echo "iorder:"
  showMatrix(q.plan.iorder, q.preds.len)
  echo "execution:"
  showMatrix(q.plan.execution, q.vars.len, q.preds.len)

  echo "building iterators: "
  var iters = newSeq[TrieIter]()
  for i in 0..<q.preds.len:
    iters.add buildIter(q.preds[i], q.plan.iorder[i])

proc newQuery(): Query =
  new result
  result.preds = newSeq[Pred]()
  result.vars = newSeq[Var]()

when isMainModule:
  const Title = A(1)
  const Completed = A(2)

  var db = newDB([sizeof E, sizeof A, -1], [Order(0x123), Order(0x213), Order(0x231)])
  db.add(1, Title, "sun!")
  db.add(2, Title, "hey!!!")

  db.index[0].showData()
  db.index[1].showData()

  var q = newQuery()

  q.pred(db, variable("?e"), constant(Title), variable("?title"))
  q.pred(db, variable("?e"), constant(Completed), variable("?completed"))
  # q.pred(db, variable("?completed"))
  # q.pred(db, variable("?title"))
  q.select("?title", "?completed")
  q.prepare
  q.execute
