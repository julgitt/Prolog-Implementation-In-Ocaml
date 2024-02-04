(*    NAWRACANIE I MUTOWALNY STAN    *)  
open BacktrackStateMonadSig
open State


module BacktrackStateMonad : BacktrackStateMonadSig = struct
  include State
  include Stack

  type 'a t = State.state Stack.IntMap.t -> 'a * State.state Stack.IntMap.t


(*        Backtracking      *)
  let backtrack_goals () map =
    let (_, map)    = Stack.pop () map in
    let (empty, _)  = Stack.is_empty () map in
    if (empty)
      then ([], map)
      else
        let prev_goals = State._get_goals map in
        _temp_substitutions := [];
        (prev_goals, map)
    

  let initialize program variables _ =
    let new_state = State._make_state program variables program [] in
    Stack._initialize_stack new_state


    
  (*      Monads operators      *)
    let return x map = (x, map)
    
    let (let*) m f map = m map |> fun (a, map) -> f a map

    let (let+) m f map = m map |> fun (a, map) -> (f a, map)
    
    let run () computation =
      let res, _ = computation Stack.IntMap.empty in
      res

end

module BSM = BacktrackStateMonad