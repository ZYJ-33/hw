open Format
(* module Reference: REFERENCE = struct *)

module Type =
struct
  type t
    = Bool
    | Unit
    | Fun of t * t
    | Ref of t

  let rec equals (t1, t2) =
      match (t1, t2) with
        | (Bool, Bool) -> true
        | (Fun (s1, s2), Fun (s3, s4)) ->
            equals (s1, s3) && equals (s2, s4)
        | (Ref s1, Ref s2) ->
            equals (s1, s2)
        | (Unit, Unit) -> true
        | _ -> false

  let rec toString t =
      match t with
        | Bool -> "bool"
        | Unit -> "unit"
        | Fun (t1, t2) ->
            toString t1 ^ " -> " ^ toString t2
        | Ref t ->
            toString t ^ " ref"

end (* structure Type *)

(* we design a language with reference *)
type t
  = True
  | False
  | If of t * t * t
  | Var of string
  | Abs of string * Type.t * t
  | App of t * t
  | Ref of t          (* ref e *)
  | Deref of t        (* !e *)
  | Assign of t * t   (* t1 := t2 *)
  | Address of string (* l *)
  | Unit

exception TypeError

(* generate fresh variables when called *)
let counter = ref 0

(* val fresh : unit -> string = <fun> *)
let fresh () =
    let n = !counter in
        let _ = (counter := n + 1) in
            "x_" ^ (string_of_int n)

exception NoRule
(*
* val alpha : string * string * t -> t = <fun>
 * alpha converting an expression
 * [x |-> s] t
 *)
let rec alpha (x, s, t) =
    match t with
    | True -> raise NoRule
    | False -> raise NoRule
    | If (_, _, _) -> raise NoRule
    | Var y ->
        if x = y
        then Var s
        else t
    | Abs (y, ty, t') ->
        if x = y
        then t
        else Abs (y, ty, alpha (x, s, t'))
    | App (t1, t2) ->
        App (alpha (x, s, t1), alpha (x, s, t2))
    | Ref _ -> raise NoRule
    | Deref _ -> raise NoRule
    | Assign (_, _) -> raise NoRule
    | Address _ -> raise NoRule
    | Unit -> raise NoRule

exception AddYourCodeHere

(*
 *  Exercise 1: complete the code for the following subst function.
 *  As an example, We have given a partial implementation.
 *
 * val subst : string * t * t -> t = <fun>
 * [x |-> s] t
 *)
let rec subst (x, s, t) =
    match t with
    | True -> raise NoRule
    | False -> raise NoRule
    | If (guard, tb, fb) -> 
        If(guard, subst(x, s, tb), subst(x, s, fb))
    | Var x' ->
          if x == x'
          then s
          else t
    | Abs (y, ty, t') ->
        if x = y	(*bound, remain the same*)
        then t
        else let f = fresh () in
                Abs (f, ty, subst (x, s, alpha (y, f, t')))
    | App (t1, t2) ->
        App (subst (x, s, t1), subst (x, s, t2))
    | Ref _ -> raise NoRule
    | Deref _ -> raise NoRule
    | Assign (_, _) -> raise NoRule
    | Address _ -> raise NoRule
    | Unit -> raise NoRule

let rec check (env, t): Type.t =
  match t with
      | True -> Type.Bool
      | False -> Type.Bool
      | If (t1, t2, t3) ->
         (let ck = check (env, t1) in
            (match ck with
              | Type.Bool ->
                let ty2 = check (env, t2) in
                  let ty3 = check (env, t3) in
                    (if Type.equals (ty2, ty3)
                      then ty2
                      else raise TypeError)
              | _ -> raise TypeError))
      | Var x -> env x
      | Abs (x, ty, t) ->
          let ty' = check ((fun y -> 
                              if x = y
                                then ty
                                else env y)
                            , t)
          in  Type.Fun (ty, ty')
      | App (t1, t2) ->
        let ty1 = check (env, t1) in
          let ty2 = check (env, t2) in
           ( match ty1 with
              | Type.Fun (ty1', ty2') ->
                  (if Type.equals (ty1', ty2)
                  then ty2'
                  else raise TypeError)
              | _ -> raise TypeError
           )

      | Ref t' ->
        let ty = check (env, t')
        in ( Type.Ref ty )

      | Deref t' ->
        let ty = check (env, t')
        in (match ty with
              | Type.Ref ty -> ty
              | _ -> raise TypeError)

      | Assign (t1, t2) ->
        let left = check (env, t1) in
          let right = check (env, t2) in
            (match left with
              | Type.Ref ty -> 
                  (if Type.equals (ty, right)
                  then Type.Unit
                  else raise TypeError)
              | _ -> raise TypeError)
      | Address _ -> Type.Unit
      | Unit -> Type.Unit


let typeCheck t = check ((fun _ -> raise TypeError), t)

(*
 * to simplify the interface of the eval
 * function, we can make the heap global, 
 * instead of an argument to this function.
 *)
exception BadAddress

module Heap =
struct
  let counter = ref 0
  let fresh () =
      let n = !counter in
          let _ = counter := !counter + 1 in
             "x_" ^ string_of_int n

  type heap = (string -> t) ref

  let heap: heap = ref (fun _ -> raise BadAddress)

  let alloc (t) =
      let newAddress = fresh () in
          let _ = heap := (fun y ->
                            (if y = newAddress
                              then t else (!heap) y))
          in  newAddress

  let lookup (x) = (!heap) x

  let update (x, t) =
      let _ = heap := (fun y ->
                              (if y=x
                                then t
                                else (!heap) y))
      in ()
end (* structure Heap *)

let isValue t =
    match t with
      | True -> true
      | False -> true
      | Abs _ -> true
      | Address _ -> true
      | Unit -> true
      | _ -> false


(*
 *  Exercise 2: complete the code for the following eval function.
 *  As an example, We have given a partial implementation.
*)
let rec eval t =
    match t with
      | Var _ -> raise NoRule
      | If (True, t2, _) -> t2
      | If (False, _, t3) -> t3
      | If (t1, t2, t3) ->
            If (eval t1, t2, t3)

      | Abs (_, _, _) ->  raise NoRule

      | App (Abs (x, ty, t'), y) ->
                subst (x, y, t')              (* E-APPABS *)

      | App (t1, t2) ->
            if isValue t1                (* E-APP2/E-APP1 *)
            then App(t1, eval t2)
            else App(eval t1, t2)

      | Ref t' ->
          (if isValue t'
            then Address (Heap.alloc t')    (* E-REFV *)
            else Ref (eval t'))             (* E-REF  *)

      | Deref t' ->
        (match t' with
            | Address x -> Heap.lookup x    (* E-DEREFLOC *)
            | _ -> Deref (eval t'))         (* E-DEREF *)

      | Assign (t1, t2) ->
        (match t1 with
            | Address x ->
                (if isValue t2
                  then (Heap.update (x, t2); Unit)  (* E-ASSIGN *)
                  else Assign (t1, eval t2))        (* E-ASSIGN2 *)
            | _ -> Assign (eval t1, t2))            (* E-ASSIGN1 *)

      | Address _ -> raise NoRule
      | Unit -> raise NoRule
      | _ -> raise NoRule

(*
 *  Exercise 3: complete the code for the following pp function.
 *  As an example, We have given a partial implementation.
*)
let rec pp t =
    match t with
      | True -> printf "True"
      | False -> printf "False"
      | If (t1, t2, t3) ->
          (printf "If(";
           pp t1; printf ", ";
           pp t2; printf ", ";
           pp t3; printf ")")

      | Var x ->
            printf "var x"
      | Abs (x, ty, t') ->
            (printf "Abs (x, ty)";
            pp t')
      | App (t', t'') ->
            (printf "App(";
            pp t';
            pp t'';
            printf ")")
      | Ref r ->
            printf "ref r"
      | Deref r ->
            printf "deref r"
      | Assign (t1, t2) ->
          (pp t1; printf " := "; pp t2)
      | Address x -> print_string x
      | Unit -> printf "()"

let rec evalAll t =
    try
        let t' = (eval t)
            in evalAll t'
    with NoRule -> t

(* ----- unit test ----- *)

let t1 = Ref Unit
let ty1 = typeCheck (t1)
let _ = print_string "t1: "
let _ = print_string (Type.toString ty1)
let _ = print_string "\n"

let t2 = Abs ("x", Type.Ref Type.Unit, Deref (Var "x"))
let ty2 = typeCheck t2
let _ = print_string "t2: "
let _ = print_string (Type.toString ty2)
let _ = print_string "\n"

let t3 = App (t2, t1)
let ty3 = typeCheck t3
let _ = print_string "t3: "
let _ = print_string (Type.toString ty3)
let _ = print_string "\n"

let t4 = Assign(Ref Unit, Deref (Ref Unit))
let r4 = evalAll t4
let _ = print_string "t4: "
let _ = pp r4
let _ = print_string "\n"

let t5 = Ref True
let ty5 = typeCheck (t5)
let _ = print_string "t5: "
let _ = print_string (Type.toString ty5)
let _ = print_string "\n"

let t6 = Deref (Ref t3)
let ty2 = typeCheck (t6)
let _ = print_string "t6: "
let _ = print_string (Type.toString ty2)
let _ = print_string "\n"
(* end *)
