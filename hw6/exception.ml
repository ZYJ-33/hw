open Format

(* Type definition *)
module Type =
struct
    type t
      = Any
      | Int

    let equals(t1, t2) =
    match (t1, t2) with
     | (Any, Any) -> true
     | (Int, Int) -> true
     | _ -> false

    let toString t =
    match t with
     | Any -> "Any"
     | Int -> "Int"
end

type t
  = Num of int              (* int type *)
  | Add of t * t            (* add operation *)
  | Throw                   (* throw exception *)
  | Try of t * t            (* handle exception *)


exception TypeError
exception NoRule
exception AddYourCodeHere

(*
 *  Exercise 1: complete the code for the following typeCheck function.
 *  As an example, We have given a partial implementation.
 *
 *  typeCheck: return type of the entered term.
 *)

let rec typeCheck t: Type.t =
    match t with
        | Num _ -> Type.Int
        | Add (left, right) ->
            let lt = typeCheck(left) in
                let rt = typeCheck(right) in
                    if rt = Type.Int && lt = Type.Int
                    then Type.Int
                    else Type.Any
        | Throw -> Type.Any
        | Try (t1, t2) ->
             let ty = typeCheck t1 in
                match ty with
                | Type.Any -> typeCheck t2
                | Type.Int -> Type.Int

let isValue t =
    match t with
        | Num _ -> true
        | _ -> false

(*
 *  Exercise 2: complete the code for the following eval function.
 *
 *  eval: evaluate the entered term according to the rules.
 *)
(*
let rec evalAll e:t =
    match e with
    | Num n -> e
    | Add (left, right) ->
                 let ln = evalAll left in
                    match ln with
                    | Num n1 -> let rn = evalAll right in
                                match rn with
                                | Num n2 -> Num (n1+n2)
                                | Throw -> rn
                                | _ -> raise NoRule
                    | Throw -> ln
                    | _ -> raise NoRule
    | Throw -> e
    | Try (t1, t2) ->
                  let le = evalAll t1 in
                     match le with
                     | Num n1 -> le
                     | Throw -> evalAll t2
                     | _ -> raise NoRule
*)

let rec eval e:t =
    match e with
    | Num n -> raise NoRule
    | Throw -> raise NoRule
    | Try (t1, t2) ->
                  (
                  match t1 with
                  | Throw -> t2
                  | Num n -> t1
                  | _ -> Try(eval t1, t2)
                  )
    | Add (t1, t2) ->
                  (
                  match t1 with
                  | Throw -> raise NoRule
                  | Num n1 ->(
                              match t2 with
                              | Throw ->raise NoRule 
                              | Num n2 -> Num(n1+n2)
                              | _ -> Add(t1, eval t2)
                              )
                  | _ -> Add(eval t1, t2)
                  )

   
(* print the term *)
let rec pp t =
    match t with 
        | Num n ->
            print_int n
        | Add (t1, t2) ->
            (printf "(";
            pp t1; printf " + "; pp t2;
            printf ")" )
        | Throw ->
            printf "throw"
        | Try (t1, t2) ->
            (printf "(try "
            ; pp t1
            ; printf " catch "
            ; pp t2
            ; printf ")")

let rec evalAll t =
  try
    let t' = (eval t)
      in evalAll t'
  with NoRule -> t


(* ------ do not change unit tests ------ *)
let t1 = Num 8                               (* print term *)
let _ = printf "t1: "
let _ = (pp t1; print_string "\n")
let ty1 = typeCheck t1                       (* print type *)
let _ = (printf "ty1: "; print_string (Type.toString ty1))
let _ = print_string "\nr1: "
let r1 = evalAll t1                          (* print eval *)
let _ = (pp r1; print_string "\n\n")

let t2 = Try (Try (Throw, Num 3), Num 5)
let _ = printf "t2: "
let _ = (pp t2; print_string "\n")
let ty2 = typeCheck t2
let _ = (printf "ty2: "; print_string (Type.toString ty2))
let _ = print_string "\nr2: "
let r2 = evalAll t2
let _ = (pp r2; print_string "\n\n")

let t3 = Throw
let _ = printf "t3: "
let _ = (pp t3; print_string "\n")
let ty3 = typeCheck t3
let _ = (printf "ty3: "; print_string (Type.toString ty3))
let _ = print_string "\nr3: "
let r3 = evalAll t3
let _ = (pp r3; print_string "\n\n")

let t4 = Try (Try (Add (Num 1, Num 3), Add (Num 2, Num 4)), Add (Num 3, Num 5))
let _ = printf "t4: "
let _ = (pp t4; print_string "\n")
let ty4 = typeCheck t4
let _ = (printf "ty4: "; print_string (Type.toString ty4))
let _ = print_string "\nr4: "
let r4 = evalAll t4
let _ = (pp r4; print_string "\n\n")

let t5 = Add (Num 3, Num 5)
let _ = printf "t5: "
let _ = (pp t5; print_string "\n")
let ty5 = typeCheck t5
let _ = (printf "ty5: "; print_string (Type.toString ty5))
let _ = print_string "\nr5: "
let r5 = evalAll t5
let _ = (pp r5; print_string "\n")
(* end *)
