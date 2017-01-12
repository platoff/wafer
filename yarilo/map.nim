
import value
import utils

type
  PMapEntry = ptr MapEntry
  MapEntry = object
  # The entry's key, or UNDEFINED_VAL if the entry is not in use.
    key: Value

  # The value associated with the key. If the key is UNDEFINED_VAL, this will
  # be false to indicate an open available entry or true to indicate a
  # tombstone -- an entry that was previously in use but was then deleted.
    value: Value

  TMap = object
    obj: TObj
    capacity: uint32
    count: uint32
    entries: HugeArray[MapEntry]


proc newMap*(gc: GC, map: var ObjMap) =
  map = cast[ObjMap](gc.allocate(TMap))

const
  MinCapacity = 16
  GrowFactor = 2
  MapLoadPercent = 75

# Looks for an entry with [key] in an array of [capacity] [entries].

# If found, sets [result] to point to it and returns `true`. Otherwise,
# returns `false` and points [result] to the entry where the key/value pair
# should be inserted.
proc findEntry(entries: HugeArray[MapEntry], capacity: int, 
  key: Value, resultEntry: var PMapEntry): bool =
  # If there is no entry array (an empty map), we definitely won't find it.
  if capacity == 0:
    return false
  
  # Figure out where to insert it in the table. Use open addressing and
  # basic linear probing.
  let startIndex = int(hashValue(key)) mod capacity
  var index = startIndex
  
  # If we pass a tombstone and don't end up finding the key, its entry will
  # be re-used for the insert.
  var tombstone: PMapEntry = nil
  
  # Walk the probe sequence until we've tried every slot.
  while true:
    let entry = addr entries[index]
    
    if isUndefined(entry.key):
      # If we found an empty slot, the key is not in the table. If we found a
      # slot that contains a deleted key, we have to keep looking.
      if isFalse(entry.value):
        # We found an empty slot, so we've reached the end of the probe
        # sequence without finding the key. If we passed a tombstone, then
        # that's where we should insert the item, otherwise, put it here at
        # the end of the sequence.
        resultEntry = if tombstone != nil: tombstone else: entry
        return false
      else:
        # We found a tombstone. We need to keep looking in case the key is
        # after it, but we'll use this entry as the insertion point if the
        # key ends up not being found.
        if tombstone == nil:
          tombstone = entry
    elif entry.key == key:
      # We found the key.
      resultEntry = entry
      return true

    # Try the next slot.
    index = (index + 1) mod capacity
    if index == startIndex:
      break
  
  # If we get here, the table is full of tombstones. Return the first one we
  # found.
  assert(tombstone != nil, "Map should have tombstones or empty entries.")
  resultEntry = tombstone

# Inserts [key] and [value] in the array of [entries] with the given
# [capacity].
# 
# Returns `true` if this is the first time [key] was added to the map.
proc insertEntry(entries: HugeArray[MapEntry], capacity: int; key, value: Value): bool =
  var entry: PMapEntry
  if findEntry(entries, capacity, key, entry):
    # Already present, so just replace the value.
    entry.value = value
    result = false;
  else:
    assert(entry != nil, "Should ensure capacity before inserting.")
    entry.key = key
    entry.value = value
    result = true;

# Updates [map]'s entry array to [capacity].
proc resizeMap(gc: GC, map: var TMap, capacity: int) =
  # Create the new empty hash table.
  let entries = gc.allocate(MapEntry, capacity)
  for i in 0..<capacity:
    entries[i].key = UndefinedVal
    entries[i].value = FalseVal

  # Re-add the existing entries.
  if map.capacity > 0u32:
    for i in 0..<map.capacity:
      let entry = addr map.entries[i]
      # Don't copy empty entries or tombstones.
      if entry.key.isUndefined: 
        continue
      discard insertEntry(entries, capacity, entry.key, entry.value)

  # Replace the array.
  gc.deallocate(map.entries)
  map.entries = entries
  map.capacity = uint32(capacity)

proc `[]`*(map: ObjMap, key: Value): Value =
  let self = cast[ptr TMap](map)
  var entry: PMapEntry
  if findEntry(self.entries, int(self.capacity), key, entry): entry.value
  else: UndefinedVal

proc mapSet*(gc: GC, map: var TMap; key, value: Value) =
  # If the map is getting too full, make room first.
  if map.count + 1 > map.capacity * MapLoadPercent div 100:
    # Figure out the new hash table size.
    let capacity = max(int(map.capacity) * GrowFactor, MinCapacity)
    gc.resizeMap(map, capacity)

  if insertEntry(map.entries, int(map.capacity), key, value):
    inc map.count

proc mapSet*(gc: GC, map: ObjMap; key, value: Value) =
  gc.mapSet(cast[ptr TMap](map)[], key, value)

proc mapSet*(gc: GC, map: ObjMap; key: Value, value: Rooted) =
  gc.mapSet(cast[ptr TMap](map)[], key, value[].asVal)
  
