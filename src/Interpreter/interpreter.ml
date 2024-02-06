open Ast
open BacktrackStateMonad.BacktrackStateMonad
open Arithmetic
open Clauses
open Unification

let rec solve goals =
  let* _ = set_goals goals in
  let* goals = get_goals () in
  match goals with
  | [] -> return true
  | g :: gs ->
    match g with
    | Sym ("is", _) -> handle_arithmetic_goal g gs
    | _ -> handle_non_arithmetic_goal g gs goals
  
and backtrack () = 
  let* new_goals = backtrack_goals () in
  match new_goals with
  | [] -> return false
  | _ -> solve new_goals

and handle_arithmetic_goal g gs =
  let* is_unified = evaluate_term g in
  if is_unified then(
    let* () = set_substitutions () in
    let* () = restore_clauses () in
    solve (gs)
  ) else backtrack ()

and handle_non_arithmetic_goal g gs goals = 
  let* clause = select_clause () in
  match clause with
  | Some c ->
    let (h, b) = refresh_clause c goals in
    let* is_unified = unify h g in
    if is_unified then
        let* () = set_substitutions () in
        let* () = restore_clauses () in
        solve (b @ gs)
      else solve goals
  | None -> backtrack ()
    

let evaluate queries =
  let* is_solved = solve queries in
  let* solution = get_substitutions () in
  return (is_solved, solution)