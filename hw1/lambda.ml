open Format

type t 
  = Var of string
  | Abs of string * t
  | App of t * t
           
exception AddYourCodeHere
          
(* whether or not a term is a value. 
 * According to the definition, there is only
 * a unique value, the lambda abstraction.
 *)
let isValue t =
    match t with
    | Abs _ -> true
    |  _ -> false
             
exception NoRule

(* generate fresh variables when called *)
let counter = ref 0

let fresh () =
    let n = !counter in 
        let _ = (counter := n + 1) in 
            "x_" ^ (string_of_int n)


(* alpha converting an expression *)
(* [x |-> s] t *)
let rec alpha (x, s, t) =
    match t with
    | Var y -> 
        if x = y
        then Var s
        else t
    | Abs (y, t') ->
        if x = y
        then t     
        else Abs (y, alpha (x, s, t'))
    | App (t1, t2) ->
        App (alpha (x, s, t1), alpha (x, s, t2))


(* [x |-> s] t *)          
let rec subst (x, s, t) =
    match t with
    | Var y ->
        if x = y
        then s
        else t
    | Abs (y, t') ->
        if x = y	(*bound, remain the same*)
        then t
        else let f = fresh () in  
                Abs (f, subst (x, s, alpha (y, f, t')))
    | App (t1, t2) ->
        App (subst (x, s, t1), subst (x, s, t2))
(* val subst : string * t * t -> t = <fun> *)


(* one-step evaluator *)
let rec eval t =
    (*
        Todo: complete the code of eval function.
    *)
    match t with
    | Var x -> t
    | Abs (x, t1) -> t
    | App(t1, t2) ->
          let x = eval t2 in
            match t1 with
            | Abs (sx, st) -> 
                subst(sx, x, st)
            | _ -> t

         

let rec pp t =
    match t with
    | Var x -> 
        print_string x
    | Abs (x, t1) -> 
        (printf "\\lambda "
        ; print_string x
        ; printf ".("
        ; pp t1
        ; printf ")")
    | App (t1, t2) ->
        (printf "("; pp t1; printf ") "; printf "("
       ; pp t2; printf ")")

let rec evalAll t =
    (*
        Todo: complete the code of evalAll function.
    *)
    match t with
    | Var x -> t
    | Abs (x, t1) -> t
    | App(t1, t2) ->
      let x = eval t2 in
         match t1 with
            | Abs (sx, st) -> 
                subst(sx, x, st)
            | _ -> t


(* a unit test *)
let unit_test () =
    let _ = printf "\x1B[32m --------------Lambda: unit test--------------\x1B[0m\n" in
    let omega = Abs ("x", (App (Var "x", Var "x")))in
    let _ = (pp omega; printf "\n")in
    let omega2 = App (omega, omega)in
    let _ = (pp omega2; printf "\n") in
        ()

      
