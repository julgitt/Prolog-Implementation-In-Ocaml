module State = struct
   include Stack

   type state = {
      program : Ast.program;
      temp_substitutions : (Ast.variable * Ast.substitution) list;
      variables : (Ast.variable * Ast.substitution) list;
      clauses : Ast.clause list;
      goals : Ast.term list;
   }

   let make_state p t v c g = {
      program = p;
      temp_substitutions = t;
      variables = v;
      clauses = c;
      goals = g;
   }

   let print_state s =
      let open Printer in
      print_endline "Klauzule:";
      print_clauses s.clauses;
      print_endline "\nTermy";
      print_terms s.goals;
      print_endline "\nProdukcje";
      print_results s.variables;
      print_endline "\nSubstytucje";
      print_results s.temp_substitutions;
      print_endline "\n"
   
   let rec print_stack map_acc map =
      if not (Stack.IntMap.is_empty map_acc) then
         let s, map_acc = Stack.pop () map_acc in
         print_state s;
         print_stack map_acc map

  (*        State getters       *)
   let _get_program map = 
      let (state, _) = Stack.get () map in
      state.program
    
   let _get_temp_substitutions map =
      let (state, _) = Stack.get () map in
      state.temp_substitutions

   let _get_variables map =
      let (state, _) = Stack.get () map in
      state.variables

   let get_goals () map =
      let (state, _) = Stack.get () map in
      (state.goals, map)

   let get_clauses () map =
      let (state, _) = Stack.get () map in
      (state.clauses, map)

    
   let get_substitutions () map =
      let variables = _get_variables map in
      let is_substituted = fun (_, term) -> term <> None in
      let subs = List.filter is_substituted variables in
      (subs, map)

  (*      Temp Substitutions    *)
   let _push_temp_substitution sub map =
      let new_temp_subs = sub :: (_get_temp_substitutions map) in
      let new_state = make_state (_get_program map)
                                 new_temp_subs
                                 (_get_variables map)
                                 (fst (get_clauses () map))
                                 (fst (get_goals () map))
      in
      Stack.update new_state map

   let _clear_temp_substitutions () map =
      let new_state = make_state (_get_program map)
                                 []
                                 (_get_variables map)
                                 (fst (get_clauses () map))
                                 (fst (get_goals () map))
      in
      Stack.update new_state map

  (*        Substitutions       *)
   let _get_all_substitutions map = 
      let subs = fst (get_substitutions () map) in
      let temp_subs = _get_temp_substitutions map in
      subs @ temp_subs

   let _find_substitution variable substitutions =
      List.assoc_opt variable substitutions

   let _has_different_substitution pair substitutions =
      List.exists (fun (name, term) -> 
         name = (fst pair) && not (term = (snd pair))
         ) substitutions

   let _contains_variable name substitutions =
      List.exists (fun (n,_) -> n = name) substitutions

   let set_substitutions () map =
      let variables = _get_variables map in
      let temp_subs = _get_temp_substitutions map in
      let apply_substitution (var, sub) =
         match sub, _find_substitution var temp_subs with
         | None, Some new_sub -> (var, new_sub)
         | _ -> (var, sub)
      in
      let new_variables = List.map apply_substitution variables in  
      let new_state = make_state  (_get_program map)
                                 temp_subs 
                                 new_variables
                                 (fst (get_clauses () map))
                                 (fst (get_goals () map))
      in
      Stack.push new_state map

   let push_substitution (var, sub) map =
      let all_subs = _get_all_substitutions map in
      if _has_different_substitution (var, sub) all_subs then (
         let (_, new_map) = _clear_temp_substitutions () map in
         (false, new_map) 
      ) else if _contains_variable var all_subs then 
         (true, map)
      else (
         let (_, new_map) = _push_temp_substitution (var, sub) map in
         (true, new_map)
      )
    
   (*      Goals       *)
   let rec apply_substitutions goals map =
      let update_goal g =
         match g with
         | Ast.Var (name, x) when x = None -> (
            match _find_substitution name (_get_all_substitutions map) with
            | Some new_sub -> Ast.Var (name, new_sub)
            | None -> g
            )
         | Sym (name, ts) -> Sym (name, fst (apply_substitutions ts map))
         | _ -> g
      in
      (List.map update_goal goals, map)

   let set_goals goals map =  
      let new_state = make_state (_get_program map)
                                 []                        
                                 (_get_variables map)
                                 (fst (get_clauses () map))
                                 goals  
      in
      Stack.update new_state map

   (*        Clauses         *)
   let set_clauses cs map =
      let new_state = make_state (_get_program map)
                                 (_get_temp_substitutions map)
                                 (_get_variables map)
                                 cs
                                 (fst (get_goals () map))
      in
      Stack.update new_state map


   let restore_clauses () map =
      let new_state = make_state (_get_program map)
                                 (_get_temp_substitutions map)
                                 (_get_variables map)
                                 (_get_program map)
                                 (fst (get_goals () map))
      in
      Stack.update new_state map

  end
