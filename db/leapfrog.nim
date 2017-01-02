
import algorithm

type
  LeapFrog*[I] = object
    iters*: seq[I]
    p: int
    atEnd: bool

  TrieJoin*[I] = object
    depth: int
    plan: seq[seq[I]]

#
# Leapfrog
#

proc search[I](leapfrog: var LeapFrog[I]) {.inline.} =
  let k = leapfrog.iters.len
  var xs = leapfrog.iters[int((uint(leapfrog.p) - 1) mod uint(k))].key
  while true:
    let x = leapfrog.iters[leapfrog.p].key
    if x == xs:
      break
    leapfrog.iters[leapfrog.p].seek(xs)
    if leapfrog.iters[leapfrog.p].atEnd:
      leapfrog.atEnd = true
      break
    xs = leapfrog.iters[leapfrog.p].key
    leapfrog.p = (leapfrog.p + 1) mod k 

proc leapfrog*[I](iters: seq[I]): LeapFrog[I] =
  result.iters = newSeq[I]()
  for i in iters:
    if i.atEnd:
      result.atEnd = true
      return
  result.iters.add iters
  result.iters.sort do (a: I, b: I) -> int: cmp(a.key, b.key)
  result.search

# proc leapfrog*[I](iters: openarray[I]): LeapFrog[I] =
#   result.iters = newSeq[I]()
#   for i in iters:
#     result.iters.add i
#   init(result)

# proc key*[I,K](leapfrog: LeapFrog[I]): K {.inline.} =
#   leapfrog.iters[leapfrog.p].key

proc atEnd*[I](leapfrog: LeapFrog[I]): bool {.inline.} = leapfrog.atEnd

proc next*[I](leapfrog: var LeapFrog[I]) =
  echo "leapfrog next: "
  # for i in leapfrog.iters:
  #   echo i
  leapfrog.iters[leapfrog.p].next
  if leapfrog.iters[leapfrog.p].atEnd:
    leapfrog.atEnd = true
  else:
    leapfrog.p = (leapfrog.p + 1) mod leapfrog.iters.len
    leapfrog.search

proc show*[I](leapfrog: LeapFrog[I]) =
    echo "Keys:"
    for i in 0..< leapfrog.iters.len:
      echo leapfrog.iters[i].key
    echo " -- "

#
# TrieJoin
#

proc init*[I](triejoin: var TrieJoin[I], plan: seq[seq[I]]) =
  triejoin.depth = -1
  triejoin.plan = plan

proc open*[I](triejoin: var TrieJoin[I]): LeapFrog[I] =
  inc triejoin.depth
  let iters = triejoin.plan[triejoin.depth] 
  for i in iters:
    i.open
  result = leapfrog iters

proc up*[I](triejoin: var TrieJoin[I]) =
  let iters = triejoin.plan[triejoin.depth] 
  for i in iters:
    i.up
  dec triejoin.depth
