open BacktrackStateMonad.BSM
module P = Printer
module U = Unification


let rec _evaluate_expr expr =
   match P.view expr with
   | Ast.Sym ("+", [l; r]) -> (_evaluate_expr l) + (_evaluate_expr r)
   | Ast.Sym ("-", [l; r]) -> (_evaluate_expr l) - (_evaluate_expr r)
   | Ast.Sym ("*", [l; r]) -> (_evaluate_expr l) * (_evaluate_expr r)
   | Ast.Sym ("/", [l; r]) -> (_evaluate_expr l) / (_evaluate_expr r)
   | Ast.Sym ("-", [x]) -> -(_evaluate_expr x)
   | Ast.Num n -> n
   | _ -> raise (Errors.Runtime_error "Invalid arithmetic expression")


let evaluate_term term =
   match P.view term with
   | Ast.Sym ("is", [l; r]) -> 
      let result = _evaluate_expr r in
      U.unify l (Ast.Num result)
   | Ast.Var _ | Ast.Num _ -> return true
   | _ -> return false