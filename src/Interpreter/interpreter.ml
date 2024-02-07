open Ast
open BacktrackStateMonad.BSM
module P = Printer
module U = Unification
module A = Arithmetic
module C = Clauses

let rec solve goals =
   let* goals = apply_substitutions goals in
   let* _ = set_goals goals in
   match goals with
   | [] -> return true
   | goal :: rest_goals ->
       match goal with
      | Sym ("is", _) -> handle_arithmetic_goal goal rest_goals
      | _ -> handle_non_arithmetic_goal goal rest_goals goals
      
and backtrack () = 
   let* new_goals = backtrack_goals () in
   match new_goals with
   | [] -> return false
   | _ -> solve new_goals

and handle_arithmetic_goal goal rest_goals =
   let* is_unified = A.evaluate_term goal in
   if is_unified then (
      let* () = set_substitutions () in
      let* () = restore_clauses () in
      solve (rest_goals)
   ) else backtrack ()

and handle_non_arithmetic_goal goal rest_goals goals = 
   let* clause = C.select_clause () in
   match clause with
   | Some c ->
      let (head, body) = C.refresh_clause c goals in
      let* is_unified = U.unify head goal in
      if is_unified then
         let* () = set_substitutions () in
         let* () = restore_clauses () in
         solve (body @ rest_goals)
      else solve goals
   | None -> backtrack ()
    

let evaluate queries =
  let* is_solved = solve queries in
  let* solution = get_substitutions () in
  return (is_solved, solution)