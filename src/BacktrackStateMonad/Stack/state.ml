open Ast;;
open Printer;;

module State = struct

  include Stack

  type state = {
      program       : clause list;
      variables : (variable * term option) list; 
      clauses       : clause list;
      goals         : term list;
    }

  let _make_state p v c g = {
      program       = p;
      variables     = v;
      clauses       = c;
      goals         = g;
    }
  

  let _print_state s =
    print_endline "Klauzule:";
    print_clauses (s.clauses);
    print_endline "\nTermy";
    print_terms (s.goals);
    print_endline "\nProdukcje";
    print_results (s.variables);
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

  let _temp_substitutions : ((variable * term option) list ref) = ref []

  (*        State getters       *)
  let _get_program map = 
    let (state, _) = Stack.get () map in
    state.program

  let _get_goals map =
    let state, _ = Stack.get () map in
    state.goals

  let get_clauses () map =
    let (state, _) = Stack.get () map in
    (state.clauses, map)

  let _get_variables map =
    let (state, _) = Stack.get () map in
    state.variables

  let get_substitutions () map =
    let variables = _get_variables map in
    let substitutions = List.filter (fun (_, term) -> term <> None) variables
    in
    (substitutions, map)


  (*        substitutions       *)

  let _all_substitutions map = 
    fst (get_substitutions () map) @ !_temp_substitutions 

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
    let apply_substitution (var, sub) =
      match sub with
      | None -> (
          match _find_substitution var !_temp_substitutions  with
          | Some new_sub -> (var, new_sub)
          | None -> (var, sub)
      )
      | Some _ -> (var, sub)
    in
    let new_variables = List.map apply_substitution variables in  
    let new_state = _make_state (_get_program map)
                                new_variables
                                (fst (get_clauses () map))
                                (_get_goals map) in
    Stack.push new_state map
  

  let push_substitution pair map =
    if _has_substitution pair (_all_substitutions map) then (
      _temp_substitutions := [];
      (false, map) 
      )
    else if _has_variable (fst pair) (_all_substitutions map) then 
      (true, map)
    else begin
      _temp_substitutions := pair :: !_temp_substitutions;
      (true, map)
    end

    
  (*      Goals       *)
  let rec _update_goals_variables goals map =
    let update_goal g =
      match g with
      | Var (name, x) when !x = None ->(
        match _find_substitution name (_all_substitutions map) with
        | Some new_sub -> x := new_sub;
        | None -> ()
        )
      | Sym (_, ts) -> _update_goals_variables ts map
      | _ -> ()
    in
    List.iter update_goal goals


  let set_goals goals map = 
    _update_goals_variables goals map; 
    _temp_substitutions := [];
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                (fst (get_clauses () map))
                                goals                          
    in
    Stack._update new_state map


  (*        clauses         *)
  let set_clauses cs map =
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                cs
                                (_get_goals map)
    in
    Stack._update new_state map

  let restore_clauses () map =
    let new_state = _make_state (_get_program map)
                                (_get_variables map)
                                (_get_program map)
                                (_get_goals map)
    in
    Stack._update new_state map

  end
