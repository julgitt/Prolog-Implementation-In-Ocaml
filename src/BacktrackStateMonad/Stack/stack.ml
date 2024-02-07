
module Stack = struct
   module IntMap = Map.Make(Int)

   let is_empty () map =
      (IntMap.is_empty map, map)

   let get () map =
      let key = (IntMap.cardinal map) - 1 in
      let value = IntMap.find key map in
      (value, map)

   let push value map =
      let key = IntMap.cardinal map in
      let new_map = IntMap.add key value map in
      ((), new_map)

   let pop () map =
      let key = (IntMap.cardinal map) - 1 in
      let (value, _) = get () map in
      let new_map = IntMap.remove key map in
      (value, new_map)

   let update new_state map =
      let (_, new_map) = pop () map in
      let (_, new_map) = push new_state new_map in 
      ((), new_map)

   let initialize_stack new_state =
      let new_map = IntMap.singleton 0 new_state in
      push new_state new_map 
end

  