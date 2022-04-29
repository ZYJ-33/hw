module Type:
sig
    type t
      = Any                 (* for throw *)
      | Int

    val equals: t * t -> bool
    val toString: t -> string
end

type t
  = Num of int              (* int type *)
  | Add of t * t            (* add operation *)
  | Throw                   (* throw exception *)
  | Try of t * t            (* handle exception *)

val evalAll: t -> t
val pp: t -> unit
val typeCheck: t -> Type.t