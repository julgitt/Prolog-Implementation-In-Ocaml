open Printer
open Unification
open BacktrackStateMonad.BacktrackStateMonad

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