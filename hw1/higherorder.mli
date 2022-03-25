type t
    = Num of int
    | Abs of (t -> t)
    | App of t * t

val eval: t -> t
val evalAll: t -> t
val pp: t -> unit
val unit_test: unit -> unit

