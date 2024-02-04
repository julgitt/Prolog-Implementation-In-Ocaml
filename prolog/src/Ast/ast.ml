type symbol = string
type variable = string

type var = term option ref
and term =
  | Var of variable * var
  | Sym of symbol * term list
  | Num of int 

type clause = clause_data
and clause_data =
  | Fact of term
  | Rule of term * term list

type program = clause list

type query   = 
  | Query of term list
  | Filepath of string
