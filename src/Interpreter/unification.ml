open Ast
open BacktrackStateMonad.BSM
module P = Printer

(*let rec _contains_var x t =
   let rec check_lists ts =
      match ts with
      | [] -> return false
      | head :: rest ->
         let* contains = _contains_var x head in
         if contains then return true
         else check_lists rest
   in
   match P.view t with
   | Var (name, _) -> let* c = contains_substitution name in
      return (not c)
   | Sym (_, ts) -> check_lists ts
   | Num _ -> return false*)


let rec unify t1 t2 =
   let rec unify_lists ts1 ts2 =
      match ts1, ts2 with
      | [], [] -> return true
      | t1 :: rest1, t2 :: rest2 ->
         let* is_unified = unify t1 t2 in
         if is_unified then unify_lists rest1 rest2
         else return false
      | _ -> return false
   in
   let t1, t2 = P.view t1, P.view t2 in
   match t1, t2 with
   | Num x, Num y -> return (x = y)
   | Var(n1, x), Var(_, y) when x = y -> 
      push_substitution (n1, Some t2);
   | Var (n, _), t | t, Var (n, _) ->
      (*let* contains = _contains_var x t in
      if contains then return false
      else *)push_substitution (n, Some t)
   | Sym(h1, ts1), Sym(h2, ts2) when _compare_symbols h1 ts1 h2 ts2 ->
      unify_lists ts1 ts2
   | _ -> return false

and _compare_symbols h1 ts1 h2 ts2 = 
   h1 = h2 && List.length ts1 = List.length ts2