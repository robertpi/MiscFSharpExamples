// Parser generators typically produce numbers represented by values in an F# [Algebraic Data Type].  For example, the following is taken (with permission) from the F# sample samples\fsharp\Parsing: 
module Ast

type expr = 
| Val of string 
| Int of int
| Float of float
| Decr of expr
 
type stmt = 
| Assign of string * expr
| While of expr * stmt
| Seq of stmt list
| IfThen of expr * stmt
| IfThenElse of expr * stmt * stmt
| Print of expr
 
type prog = Prog of stmt list


