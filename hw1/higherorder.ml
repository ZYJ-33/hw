open Format

exception AddYourCodeHere

type t
  = Num of int
  | Abs of (t -> t)
  | App of t * t

exception NoRule

let isValue t = 
    match t with
    | App _ -> false
    | _ -> true

let rec eval t =
        match t with
        | Num i -> i
        | Abs f -> f
        | App(t1, t2) ->
          (
                let r = eval t2 in
                    let l = eval t1 in
                        r l    
          )
    
let rec pp t =
    match t with 
    | Num i -> 
        (let m = string_of_int i in
            print_string m)
    | Abs f -> 
        printf "Abs"
    | App (t1, t2) ->
        (printf "("
       ; pp t1
       ; printf ") ("
       ; pp t2
       ; printf ")")

let rec evalAll t = 
    try
    let t' = eval t
        in evalAll t'
    with NoRule -> t

(* unit test *)
let unit_test () =
    let _ = printf "\x1B[32m -------------- Higherorder : unit test--------------\x1B[0m\n" in
    let e = App (App (Abs (fun x ->
                          Abs (fun y ->
                                  x)), 
                  Num 3), 
             Num 4) in
    let _ = pp e in
    let _ = printf "\n" in
    let _ = evalAll e in
        ()


