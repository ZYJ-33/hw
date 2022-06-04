open Format

exception AddYourCodeHere
exception TypeError of string

module Type =
struct
    type t
        = Record of (string * t) list
        | Top
        | String            (* Represents the basic types used in practice, 
                                such as int, float and so on. *)
        | Arrow of t * t
     (* print the type *)
    let rec pp t =
    match t with
        | Record l -> 
            (
                    let rec ppfields l =
                    match l with
                        | [] -> ()
                        | (name, t)::xs ->
                            (print_string name
                            ; printf " : "
                            ; pp t
                            ; printf ", "
                            ; ppfields xs)
                    in
                    (printf "{"
                    ; ppfields l
                    ; printf "}")
             )
        | Top -> 
            printf "Top"
        | String -> 
            printf "String"
        | Arrow (t1, t2) -> 
            (pp t1;
             printf " -> ";
             pp t2
            )
    
end 
(* structure Type *)


type t
    = Record of (string * t) list
    | Proj of t * string
    | String of string    (* Represents the basic types used in practice, 
                            such as int, float and so on. *)          
    | Var of string
    | Abs of string * Type.t * t
    | App of t * t
 
let rec __subtype (sr, tr) =
        match sr with
                | [] -> true
                | (sname, st)::sxs ->
                             match tr with
                             | [] -> false
                             | (tname, tt)::txs ->
                                             if sname = tname && st = tt
                                             then __subtype(sxs, txs)
                                             else false
(* Check if S is a subtype of T *)
let rec subtype (s, t) =
        match s with
        | Type.Record sl ->
                        match t with 
                                | Type.Record tl ->
                                                __subtype(sl, tl) 
                                | _ -> false
        | _ -> false

let rec typeof(record, str) = 
        match record with
        | [] -> raise (TypeError "error")
        | (name, t)::xs ->
                        if name = str
                        then t
                        else typeof(xs, str)


let rec check (env, t) = 
    match t with
        | App(t1, t2) ->
                 let ft = check(env, t1) in
                 (
                    match ft with
                    | Type.Arrow(para_t, ret_t) -> ret_t 
                    | _ -> raise (TypeError "error")
                 )
                            
        | Abs(para_name, para_type, body) -> 
          Arrow(
                para_type,
                check
                (
                                      (
                                        fun y ->
                                        if y = para_name
                                        then para_type
                                        else env y         
                                      ),
                                      body
                )
               )
        | Var(s) -> env(s)
        | String(s) -> Type.String
        | Proj(var, string) -> 
                        match var with
                        | Var(name) ->let res = env(name) in
                                      match res with
                                      | Record t -> typeof(t, string)
                                      | _ -> raise (TypeError "error")
                                                      
                        | _ -> raise (TypeError "error")

        | Record a -> Type.Record(a)
                        
                        
(*
 *  Exercise : 
 *      complete the following 2 function: subtype() and check(), 
 *      to make the following typeCheck function successfully
 *      return type of the entered term.

 *  Tip :
 *      If you lack ideas, you can refer to assignment 4: Type. 
 *)
(* Return type of the entered term *)
let typeCheck t = 
    check ((fun _ -> raise (TypeError "var not found")), t)

(* print the term *)
let rec pp t =
    match t with 
        | Record l ->
            (let rec ppfields l =
                    match l with
                        | [] -> ()
                        | (name, t)::xs ->
                            (print_string name
                            ; printf " = "
                            ; pp t
                            ; printf ", "
                            ; ppfields xs)
            in
                (printf "{"
                ; ppfields l
                ; printf "}"))
        | Proj (t, name) ->
            (pp t
            ; printf "."
            ; print_string name)
        | String s ->
            print_string s
        | Var x ->
            print_string x
        | Abs (x, ty, t) ->
            (printf "λ "
            ; print_string x
            ; printf ": "
            ; Type.pp ty
            ; printf ". "
            ; pp t)
        | App (t1, t2) ->
            (printf "("
            ; pp t1
            ; printf ") ("
            ; pp t2
            ; printf ")")

(* unit test *)
let _ = printf "\n\x1B[32m -------------- unit test--------------\x1B[0m\n" 

let e1 = App (
                Abs (
                        "x",
                         Type.Record [("name", Type.String)],
                         Proj (Var "x", "name")
                    ),
                Record [("age", String "20"); ("name", String "Bob")]
              )
let _ = (pp e1; printf "\n")
let ty1 = typeCheck e1
let _ = (Type.pp ty1; printf "\n\n")


let e2 = App (
                Abs (
                        "y",
                        Type.Record [("name", Type.String); ("gender", Type.String)],
                        Var "y"
                    ),
                Record [("age", String "20"); ("name", String "Bob"); ("gender", String "male")]
             )
let _ = (pp e2; printf "\n")
let ty2 = typeCheck e2
let _ = (Type.pp ty2; printf "\n\n")


let e3 = Abs (
              "x",
              Type.Record [("name", Type.String); ("gender", Type.String)],
              Proj (Var "x", "gender")
             )
let _ = (pp e3; printf "\n")
let ty3 = typeCheck e3
let _ = (Type.pp ty3; printf "\n\n")

let e4 = String "Well done! "
let _ = (pp e4; printf "\n")
let ty4 = typeCheck e4
let _ = (Type.pp ty4; printf "\n\n")
