
module Stack = struct
  module IntMap = Map.Make(Int)

  let is_empty () map =
    (IntMap.is_empty map, map)

  let get () map =
    let key = (IntMap.cardinal map) - 1 in
    let v = IntMap.find key map in
    (v, map)

  let push v map =
    let key = IntMap.cardinal map in
    let new_map = IntMap.add key v map in
    ((), new_map)

  let pop () map =
    let key = (IntMap.cardinal map) - 1 in
    let (v, _) = get () map in
    let new_map = IntMap.remove key map in
    (v, new_map)

  let _update new_state map =
    let (_, new_map) = pop () map in
    let (_, new_map) = push new_state new_map in 
    ((), new_map)

  let _initialize_stack new_state =
    let new_map = IntMap.singleton 0 new_state in
    push new_state new_map 
end

  