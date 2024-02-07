open Ast
open BacktrackStateMonad.BSM
module P = Printer


let rec rename_variables gs term = 
   match P.view term with
   | Var(current_name, x) ->
      Var(fresh_variable current_name gs, x)
   | Sym(f, ts) -> Sym(f, List.map (rename_variables  gs) ts)
   | Num _ -> term

and fresh_variable name gs =
   let rec create_new_name n =
      let new_name = n ^ "`" in
      if if_variable_exist new_name gs then 
         create_new_name new_name
      else new_name
   in
   create_new_name name

and if_variable_exist name gs =
   let rec contains_var_name g =
      match P.view g with
      | Var (n, _) -> n = name
      | Sym (_, ts) -> List.exists contains_var_name ts
      | Num _ -> false
   in
   List.exists contains_var_name gs


let refresh_clause clause goals = 
   match clause with
   | Fact t ->
      let head = rename_variables goals t in
      let body = [] in 
      (head, body)
   | Rule (t, ts) ->
      let head = rename_variables goals t in
      let body = List.map (rename_variables goals) ts in 
      (head, body)

    
let select_clause () = 
   let* predicate_base = get_clauses () in
   match predicate_base with
   | [] -> return None
   | c :: rest -> 
      let* _ = set_clauses rest in
      return (Some c)