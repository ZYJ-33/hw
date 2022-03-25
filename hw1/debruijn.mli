(* the basic problems with the explicit naming and the
 * substitution-based semantics in "lambda.ml" is that:
 *   1. it's really slow, for we would have to compare
 *      string equality along the way; and
 *   2. it's really stupid, for we have to rename the
 *      lambda binding variable each time we 
 *      substitute a term in a lambda.
 * So our question is can we use a more fancy computation
 * model which does not suffer from these headaches?
 * One answer to this question is to use the so-called
 * De Bruijn representation.
 *)

type t 
    = Var of int
    | Abs of t
    | App of t * t

val eval: t -> t
val evalAll: t -> t
val fromLambda: Lambda.t -> t
val pp: t -> unit
val toLambda: t -> Lambda.t 
val unit_test: unit -> unit

