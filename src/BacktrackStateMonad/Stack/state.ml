open Ast;;
open Printer;;

module State = struct

  include Stack

  type state = {
      program            : clause list;
      variables          : (variable * term option) list;
      clauses            : clause list;
      goals              : term list;
      temp_substitutions : (variable * term option) list;
    }

  let _make_state p v c g t = {
      program            = p;
      variables          = v;
      clauses            = c;
      goals              = g;
      temp_substitutions = t;
    }
  

  let _print_state s =
    print_endline "Klauzule:";
    print_clauses (s.clauses);
    print_endline "\nTermy";
    print_terms (s.goals);
    print_endline "\nProdukcje";
    print_results (s.variables);
    print_endline "\nSubstytucje";
    print_results (s.temp_substitutions);
    print_endline "\n";
    ()
  
    
  let rec _print_stack map_acc map =
    let empty = Stack.IntMap.is_empty map_acc in
    if (empty)
      then 
        ()
      else
        let (s,map_acc) = Stack.pop () map_acc in
        _print_state s;
        _print_stack map_acc map


  (*        State getters       *)
  let _get_program map = 
    let (state, _) = Stack.get () map in
    state.program

  let get_goals () map =
    let state, _ = Stack.get () map in
    (state.goals, map)

  let get_clauses () map =
    let (state, _) = Stack.get () map in
    (state.clauses, map)

  let _get_variables map =
    let (state, _) = Stack.get () map in
    state.variables

  let get_substitutions () map =
    (*print_endline "NOWE";
    _print_stack map map;*)
    let variables = _get_variables map in
    let substitutions = List.filter (fun (_, term) -> term <> None) variables
    in
    (substitutions, map)

  let _get_temp_substitutions map =
    let (state, _) = Stack.get () map in
    state.temp_substitutions

  (*      temp substitutions    *)

  let _push_temp_substitution sub map =
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                (fst (get_clauses () map))
                                (fst (get_goals () map))
                                (sub :: (_get_temp_substitutions map))
    in
    Stack._update new_state map


  let _clear_temp_substitutions () map =
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                (fst (get_clauses () map))
                                (fst (get_goals () map))
                                []
    in
    Stack._update new_state map


  (*        substitutions       *)

  let _all_substitutions map = 
    fst (get_substitutions () map) @ (_get_temp_substitutions map)

  let _find_substitution variable substitutions =
   List.assoc_opt variable substitutions

  let _has_substitution pair substitutions =
    List.exists (fun (name, term) -> 
      name = (fst pair) && not (term = (snd pair))
      ) substitutions

  let _has_variable name subs =
   List.exists (fun (n,_) -> n = name) subs


  let set_substitutions () map =
    let variables = _get_variables map in
    let temp_subs = _get_temp_substitutions map in
    let apply_substitution (var, sub) =
      match sub with
      | None -> (
          match _find_substitution var temp_subs with
          | Some new_sub -> (var, new_sub)
          | None -> (var, sub)
      )
      | Some _ -> (var, sub)
    in
    let new_variables = List.map apply_substitution variables in  
    let new_state = _make_state (_get_program map)
                                new_variables
                                (fst (get_clauses () map))
                                (fst (get_goals () map))
                                temp_subs in
    Stack.push new_state map
  

  let push_substitution pair map =
    if _has_substitution pair (_all_substitutions map) then (
      let (_, new_map) = _clear_temp_substitutions () map in
      (false, new_map) 
      )
    else if _has_variable (fst pair) (_all_substitutions map) then 
      (true, map)
    else begin
      let (_, new_map) = _push_temp_substitution pair map in
      (true, new_map)
    end

    
  (*      Goals       *)
  let rec _update_goals_variables goals map =
    let update_goal g =
      match g with
      | Var (name, x) when x = None ->(
        match _find_substitution name (_all_substitutions map) with
        | Some new_sub -> Var (name, new_sub)
        | None -> g
        )
      | Sym (name, ts) -> Sym (name, _update_goals_variables ts map)
      | _ -> g
    in
    List.map update_goal goals


  let set_goals goals map = 
    let new_goals = _update_goals_variables goals map in 
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                (fst (get_clauses () map))
                                new_goals  
                                []                        
    in
    Stack._update new_state map


  (*        clauses         *)
  let set_clauses cs map =
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                cs
                                (fst (get_goals () map))
                                (_get_temp_substitutions map)
    in
    Stack._update new_state map

  let restore_clauses () map =
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                (_get_program map)
                                (fst (get_goals () map))
                                (_get_temp_substitutions map)
    in
    Stack._update new_state map

  end
