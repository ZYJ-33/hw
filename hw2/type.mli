(* Data type definitions *)
type ty = 
      Bool
    | Fun of ty * ty

type t =
      True
    | False
    | If of t * t * t
    | Var of string
    | Abs of string * ty * t
    | App of t * t

val tyEquals: ty * ty -> bool

val typeCheck: t -> ty
