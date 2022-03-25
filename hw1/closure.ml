open Format

type t
    = Num of int
    | Var of string
    | Abs of string * t
    | App of t * t
    (* the implicit invariant here is that
    * the secnd "t" should always be a lambda.
    *)
    | Closure of env * t 
        and env = T of (string -> t)
    
exception NoRule
exception BadExp
exception AddYourCodeHere


let isValue t =
    match t with
    | Num _ -> true
    | Closure _ -> true
    | _ -> false

let emptyEnv = T (fun _ -> raise BadExp)

let rec eval (T map, t) : env * t = 
    match t with
    | Num n -> (map, t)
    | Var x -> (map, t)
    | Abs (x, t') -> (map, t)
    | App (t1, t2) -> (
        match t1 with
        | Abs(var, t')->eval(
          eval(fun y -> (
            if(y = var)
            then eval t2
            else map y  
        ), t')
        )
        | _ -> raise NoRule
    )  
    | Closure (evn, t') -> eval(evn, t')

let rec pp t =
    match t with
    | Num n -> 
        (let m = string_of_int n in
            print_string m)
    | Var x -> 
        print_string x
    | Abs (x, t1) -> 
        (printf "\\lambda "
        ; print_string x
        ; printf ".("
        ; pp t1
        ; printf ")")
    | App (t1, t2) ->
        (printf "("
        ; pp t1
        ; printf ") ("
        ; pp t2
        ; printf ")")
    | Closure (env, t) ->
        (printf "Closure(C, "
        ; pp t
        ; printf ")") 

let rec evalAll (env, t): env * t = 
    try
        let (env', t') = eval (env, t)
        in evalAll (env', t') 
    with NoRule -> (env, t)

let unit_test () =
    let _ = printf "\x1B[32m -------------- Closure : unit test--------------\x1B[0m\n" in

let e = App (App (Abs ("x", 
                       Abs ("y",
                            Var "x")), 
                  Num 3), 
             Num 4) in

let _ = pp e in

let _ = printf "\n" in

let _ = evalAll (emptyEnv, e)  in
    ()


