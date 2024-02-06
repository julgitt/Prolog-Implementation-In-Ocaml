open Printer
open Ast
open BacktrackStateMonad.BacktrackStateMonad


let rec renameVariables gs t = 
  match t with
  | Var(current_name, x) -> (
    match x with
    | None -> Var(fresh_variable current_name gs, None)
    | Some y -> Var(current_name, Some (renameVariables gs y))
    )
  | Sym(f, ts) -> Sym(f, List.map (renameVariables  gs) ts)
  | Num x -> Num x

and fresh_variable name gs =
  let rec create_new_name n =
    let new_name = n ^ "`" in
    if if_variable_exist new_name gs
      then create_new_name new_name
      else new_name
  in
  create_new_name name

and if_variable_exist name gs =
  let rec contains_var_name g =
    match view g with
    | Var (n, _) -> n = name
    | Sym (_, ts) -> List.exists contains_var_name ts
    | Num _ -> false
  in
  List.exists contains_var_name gs


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