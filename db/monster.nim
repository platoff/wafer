

import mersenne, algorithm, db, strutils, encoding

const ROWS = 100

var tw = newMersenneTwister(556)
var lastId = 0

proc getId(): E =
  inc lastId
  lastId

const
  DatabaseName = A(1)
  Elapsed = A(2)
  Waiting = A(3)
  SQL = A(4)
  Queries = A(5)

template random(m: int): untyped =
  int(tw.getNum) %% m

proc genQuery(db: DB): E =
  result = getId()
  db.add(result, Elapsed, random(1500))
  db.add(result, Waiting, random(2) == 0)
  var query: string
  case random(10)
  of 0: query = "vacuum"
  of 1..2: query = "<IDLE> in transaction"
  else: query = "SELECT blah FROM something"
  db.add(result, SQL, query)
  
proc genDatabase(db: DB, name: string): E =
  result = getId()
  db.add(result, DatabaseName, name)
  for i in 0..random(10):       
    db.add(result, Queries, db.genQuery())

proc genData*(db: Db) =
  for i in 1..ROWS:
    discard db.genDatabase("cluster" & $i)
    discard db.genDatabase("cluster" & $i & " slave")

when isMainModule:
  import times
  const ITERS = 1000

  var monster = newDB([sizeof E, sizeof A, -1], [Order(0x123), Order(0x213), Order(0x231)])
  monster.genData()
  monster.showData(1)

  var q = newQuery()

  q.pred(monster, variable("?q"), constant(Waiting), constant(true))
  q.pred(monster, variable("?db"), constant(Queries), variable("?q"))
  q.pred(monster, variable("?db"), constant(DatabaseName), variable("?name"))
  # q.pred(db, variable("?completed"))
  # q.pred(db, variable("?title"))
  q.select("?name")
  q.prepare
  
  let start = cpuTime()
  
  for i in 0..<ITERS:
    q.execute

  echo "Iteration time: ", formatFloat((cpuTime() - start) * 1000 / ITERS, ffDecimal, 3), " ms"
