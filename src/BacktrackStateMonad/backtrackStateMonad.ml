open BacktrackStateMonadSig
module State = State.State

module BSM : BSM_Sig = struct
  include State
  include Stack

  type 'a t = State.state Stack.IntMap.t -> 'a * State.state Stack.IntMap.t

   (*        Backtracking      *)
   let backtrack_goals () map =
      let (_, map)    = Stack.pop () map in
      let (empty, _)  = Stack.is_empty () map in
      if (empty) then ([], map)
      else
         let (prev_goals, _) = get_goals () map in
         let (_, map) = _clear_temp_substitutions () map in
         (prev_goals, map)
      
   let initialize program variables _ =
      let new_state = State.make_state program [] variables program [] in
      Stack.initialize_stack new_state

  (*      Monads operators      *)
   let return x map = (x, map)
   
   let (let*) m f map = m map |> fun (a, map) -> f a map

   let (let+) m f map = m map |> fun (a, map) -> (f a, map)
   
   let run () computation =
      let res, _ = computation Stack.IntMap.empty in
      res

end
