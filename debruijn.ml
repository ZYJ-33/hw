open Format
type t 
  = Var of int
  | Abs of t
  | App of t * t

exception BadExpression

let fromLambda t =
    let rec doit (map, t) =
        match t with
        | Lambda.Var x -> 
            Var (map x)
        | Lambda.Abs (x, e) ->
            Abs (doit (
                    (fun y -> (if x = y
                                then 0
                                else (map y)+1))
                    , e))
        | Lambda.App (t1, t2) ->
            App (doit (map, t1)
                , doit (map, t2))

    in  doit ((fun _ -> raise BadExpression), t)

exception AddYourCodeHere
exception NoRule

let isValue t = 
    match t with
    | Abs _ -> true
    | _ -> false

(* [j |-> s] t *)
let rec subst (j, s, t) =
    match t with
    | Var m ->
        if m = j
        then s
        else t
    | Abs t' ->
        Abs (subst (j+1, s, t'))
    | App (t1, t2) ->
        App (subst (j, s, t1), 
             subst (j, s, t2))

(* we only consider closed-terms, so the 
 * complexity about naming context does not
 * concern us here.
 *)
let rec eval t =
    (*
        Todo: complete the code of eval function.
    *)
    match t with
    | Var m -> t
    | Abs t' -> t
    | App (t1, t2) ->
      (
          let r = eval t2 in 
             match t1 with
             | Abs t'' -> subst(0, r, t'')
             | _ -> t
      ) 
                    



let rec evalAll t =
    try
        let t' = (eval t)
            in evalAll t'
    with NoRule -> t


let counter = ref 0

let fresh () =
    let n = !counter in 
        let _ = (counter := n + 1) in 
            "x_" ^ (string_of_int n)



let toLambda t = 
    (*
        Todo: complete the code of eval function.
    *)
    let rec undoit(map, t) = 
        match t with
        | Var x ->
            Lambda.Var (map x)
        | Abs t' ->
            let x = fresh() in
            Lambda.Abs(x, undoit(func y -> (
                if y = 0
                    then x
                    else map (y-1)
            ), t'))
        | App (t1, t2) ->
            Lambda.App(undoit(map, t1), undoit(map, t2))
              
    in
    undoit((func y -> raise BadExpression), t)
   
let rec pp t =
    match t with
    | Var n -> 
        (let m = string_of_int n in
            print_string m)
    | Abs t' ->
        (printf "\\lambda.("; pp t'; printf ")")
    | App (t1, t2) ->
        (printf "("; pp t1; printf ") (";
         pp t2; printf ")")


let unit_test () =
    let _ = printf "\x1B[32m -------------- DeBruijn : unit test--------------\x1B[0m\n" in
let e = Lambda.Abs 
        ("x", 
         Lambda.Abs 
         ("y",
          Lambda.App (Lambda.Var "x",
                      Lambda.Var "y"))) in

let _ = Lambda.pp e in
let _ = printf "\n" in

let e1 = fromLambda e in
let _ = pp e1 in
let _ = printf "\n\n" in

let id = Lambda.Abs ("x", Lambda.Var "x")in
let app = Lambda.App (e, id)in
let _ = Lambda.pp app in
let _ = printf "\n" in

let e2 = fromLambda app in
let _ = pp e2 in
let _ = printf "\n\n" in

let e3 = Lambda.evalAll app in
let _ = Lambda.pp e3 in
let _ = printf "\n" in

let e4 = fromLambda e3 in
let _ = pp e4 in
let _ = printf "\n" in

let e5 = evalAll e2 in
let _ = pp e5 in
let _ = printf "\n" in

let e6 = toLambda e5 in
let _ = Lambda.pp e6 in
let _ = printf "\n" in
()





