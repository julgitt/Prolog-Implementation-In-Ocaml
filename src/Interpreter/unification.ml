open Ast
open BacktrackStateMonad.BSM
module P = Printer

let rec contains_var x t =
   match P.view t with
   | Var (_, y) -> x = y
   | Sym (_, ts) -> List.exists (contains_var x) ts
   | Num _ -> false


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
   | Var (n, x), t | t, Var (n, x) ->
      if contains_var x t then return false
      else push_substitution (n, Some t)
   | Sym(h1, ts1), Sym(h2, ts2) when compare_symbols h1 ts1 h2 ts2 ->
      unify_lists ts1 ts2
   | _ -> return false

and compare_symbols h1 ts1 h2 ts2 = 
   h1 = h2 && List.length ts1 = List.length ts2