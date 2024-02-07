val solve : Ast.term list -> (bool * ((Ast.variable * Ast.substitution) list))
                              BacktrackStateMonad.BSM.t