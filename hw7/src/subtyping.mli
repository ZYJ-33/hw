module Type:
sig
    type t
      = Record of (string * t) list
      | Top
      | String
      | Arrow of t * t
end
(* We assume that the record fields are sorted.
 * And this can be easily achieved by
 * pre-sorting the record fields.
 *)
type t
  = Record of (string * t) list
  | Proj of t * string
  | String of string
  | Var of string
  | Abs of string * Type.t * t
  | App of t * t

val pp: t -> unit
val typeCheck: t -> Type.t