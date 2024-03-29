type symbol = string
type variable = string

type substitution = term option
and term =
   | Var of variable * substitution
   | Sym of symbol * term list
   | Num of int 

type clause = clause_data
and clause_data =
   | Fact of term
   | Rule of term * term list

type program = clause list

type query = 
   | Query of term list
   | Filepath of string
