open Ast
open BacktrackStateMonad.BacktrackStateMonad
open Printer


(*          UNIFIKACJA               *)

let rec unify t1 t2 =
  let rec contains_var x t =
    match view t with
    | Var (_, y)  -> x = y
    | Sym (_, ts) -> List.exists (contains_var x) ts
    | Num _ -> false
  in

  let rec unify_lists ts1 ts2 =
    match ts1, ts2 with
    | [], [] -> return true
    | t1 :: rest1, t2 :: rest2 ->
      let* is_unified = unify t1 t2 in
      if is_unified then unify_lists rest1 rest2
      else 
        return false
    | _ -> return false
  in

  match view t1, view t2 with
  | Num x, Num y -> return (x = y)
  | Var (n1, x), Var (n2, y) when x = y -> 
    push_substitution (n1, Some (Var(n2,y)));
  | Var (n, x), t |  t, Var (n, x)->
    if contains_var x t then return false
    else push_substitution (n, Some t)
  | Sym(h1, ts1), Sym(h2, ts2) when h1 = h2 && List.length ts1 = List.length ts2 ->
    unify_lists ts1 ts2
  | _ -> return false



  (*          ARYTMETYKA               *)

let rec evaluate_arithmetic_expr expr =
  match view expr with
  | Sym ("+", [lhs; rhs]) -> (evaluate_arithmetic_expr lhs) + (evaluate_arithmetic_expr rhs)
  | Sym ("-", [lhs; rhs]) -> (evaluate_arithmetic_expr lhs) - (evaluate_arithmetic_expr rhs)
  | Sym ("*", [lhs; rhs]) -> (evaluate_arithmetic_expr lhs) * (evaluate_arithmetic_expr rhs)
  | Sym ("/", [lhs; rhs]) -> (evaluate_arithmetic_expr lhs) / (evaluate_arithmetic_expr rhs)
  | Sym ("-", [hs])       -> -(evaluate_arithmetic_expr hs)
  | Num n -> n
  | _ -> raise (Errors.Runtime_error "Invalid arithmetic expression")


let evaluate_term term =
  match view term with
  | Sym ("is", [lhs; rhs]) -> 
    let result = evaluate_arithmetic_expr rhs in
    unify lhs (Num result)
  | Var _ | Num _ -> return true
  | _ -> return false

(*          SEMANTYKA               *)

let if_variable_exist goals name =
  let rec helper g =
    match view g with
    | Var (n,_) -> n = name
    | Sym (_, ts)-> List.exists helper ts
    | Num _ -> false
  in
  List.exists helper goals


let rec renameVariables goals t = 
  let fresh_variable_name name =
    let rec create_new_name n =
      let new_name = n ^ "`" in
      if  if_variable_exist goals new_name 
        then create_new_name new_name
        else new_name
    in
    create_new_name name
  in
  match t with
  | Var(name, x) ->  
    (match x with
    | None     -> 
      Var((fresh_variable_name name), None)
      | Some y   -> Var(name, Some (renameVariables goals y))
    )
  | Sym(f, ts) -> Sym(f, (List.map (renameVariables  goals) ts ))
  | Num x -> Num x


let refresh_clause c goals = 
  match c with
  | Fact t ->
    let h = renameVariables goals t in
    let b = [] in 
    (h, b) 
  | Rule (t, ts) ->
    let h = renameVariables goals t in
    let b = List.map (renameVariables goals) ts in 
    (h, b)
          

let select_clause () = 
  let* predicate_base = get_clauses () in
  match predicate_base with
  | [] -> return None
  | c :: rest -> 
    let* _ = set_clauses rest in
    return (Some c)


let rec solve goals =
  let* _ = set_goals goals in
  let* goals = get_goals () in
  match goals with
  | []      -> return true
  | g :: gs -> (
    match g with
    | Sym ("is", _) -> 
      let* is_arithm_unified = evaluate_term g in
      if is_arithm_unified then(
        let* () = set_substitutions () in
        let* () = restore_clauses () in
        solve (gs)
      ) else (
        let* new_goals = backtrack_goals () in
        match new_goals with
        | [] -> return false
        | _  ->  solve new_goals
      )
    | _ ->
      let* clause = select_clause () in
      match clause with
      | Some c ->
        let (h, b) = refresh_clause c goals in
        let* is_unified = unify h g in
        if is_unified then
            let* () = set_substitutions () in
            let* () = restore_clauses () in
            solve (b @ gs)
            else (
              solve goals)
      | None ->
        let* new_goals = backtrack_goals () in
        match new_goals with
        | [] -> return false
        | _  ->  solve new_goals
      )

let evaluate queries =
  let* is_solved = solve queries in
  let* solution = get_substitutions () in
  return (is_solved, solution)