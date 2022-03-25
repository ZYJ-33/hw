open Format

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

exception TypeError

exception AddYourCodeHere

let rec tyEquals (ty1, ty2) =
  match (ty1, ty2) with
    (Bool, Bool) -> true
    | (Fun (s1, s2), Fun (s3, s4)) ->
      tyEquals (s1, s3) && tyEquals (s2, s4)
    | _ -> false
      
(*
    Exercise: complete the code for the following check(env, t) function.
    As an example, We have given a partial implementation, please add
    your code at the TODO to make the test samples pass correctly.
*)
let rec check (env, t): ty =
  match t with
    | True -> Bool
    | False -> Bool
    | Var x -> env x
    | If (t1, t2, t3) ->
      (
           let guard = check(env, t1) in
                match guard with
                | Bool -> (
                    match tyEquals(check(t2), check(t3)) with
                    | true -> check(t2)
                    | _ -> raise TypeError
                )
                | _ -> raise TypeError
      )

    | Abs (x, ty, t) ->
      Fun (ty, check ((fun y -> if x = y
                               then ty
                               else env y)
                      , t))
    | App (t1, t2) ->
      let right_ty = check(env, t2) in 
      match t1 with
      | Abs(s, ty, t') -> (
        if right_ty = ty
        then check((func y -> if y=s then ty else env y), t')
        else raise TypeError
      )
      | _ -> raise TypeError



let typeCheck t = 
  check ((fun x -> raise TypeError), t)

(* unit test *)
let e1 = App (Abs ("x", Bool, Var "x"), True)
let r1 = typeCheck e1

let e2 = App (Abs ("x", Fun (Bool, Bool), Var "x")
            , (Abs ("x", Bool, Var "x")))
let r2 = typeCheck e2

let e3 = App (Abs ("y", Bool, Var "y"), False)
let r3 = typeCheck e3

let e4 = Abs ("y", Bool, If (Var "y", False, True))
let r4 = typeCheck e4

let e5 = True
let r5 = typeCheck e5

let e6 = If (True, False, True)
let r6 = typeCheck e6


let _ = (if r1 != r2
        then printf "Pass\n"
        else printf "Fail\n")

let _ = (if r1 = r3
        then printf "Pass\n"
        else printf "Fail\n")

let _ = (if r2 = r4
        then printf "Pass\n"
        else printf "Fail\n")

let _ = (if r5 = r6
        then printf "Pass\n"
        else printf "Fail\n")

