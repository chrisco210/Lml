open Lambdaast
open Iast

let counter = ref 0

(** [free_var] generates a fresh variable that has not been used before.  
    It will never match a user defined variable because it includes characters
    that cannot be included in user defined variable names. *)
let free_var () : ivar = 
  counter := !counter + 1; 
  "*/" ^ (string_of_int !counter) 

let rec convert_cps_init (e : iast) : iast = 
  match e with
  | App (e1, e2) -> 
    let k = free_var () in
    let f = free_var () in
    let v = free_var () in
    Lam (k, 
         App (
           convert_cps_init e1, 
           Lam (
             f, 
             App (
               convert_cps_init e2,
               (Lam (v, App (App (Var f, Var v), Var k)))
             )
           )
         )
        )
  | Lam (v, e') -> 
    let k = free_var () in
    let k' = free_var () in
    Lam (k, 
         App (Var k, 
              Lam (v,
                   Lam (k',
                        App (
                          convert_cps_init e',
                          Var k'
                        )
                       )
                  )
             )
        )
  | Bop (b, e1, e2) -> 
    let k = free_var () in
    let n = free_var () in 
    let m = free_var () in
    Lam (k, 
         App (
           convert_cps_init e1,
           Lam (n,
                App (
                  convert_cps_init e2,
                  Lam (m, 
                       App (
                         Var k,
                         Bop (b, Var n, Var m)
                       )
                      )
                )
               )
         )
        )
  | Uop (u, e') -> failwith "Unimplemented convert_cps_init"
  | If (g, e1, e2) -> failwith "Unimplemented convert_cps_init"
  | Seq (e1, e2) -> failwith "Unimplemented convert_cps_init"
  | Ref (e) -> failwith "Unimplemented convert_cps_init"
  | While (e1, e2) -> failwith "Unimplemented convert_cps_init"
  | Assign (e1, e2) -> failwith "Unimplemented convert_cps_init"
  | Break -> failwith "Unimplemented convert_cps_init"
  | Continue -> failwith "Unimplemented convert_cps_init"
  | Var v -> 
    let k = free_var () in 
    Lam (k, App (Var k, Var v))
  | Int i -> 
    let k = free_var () in 
    Lam (k, App (Var k, Int i))
  | Bool b -> 
    let k = free_var () in 
    Lam (k, App (Var k, Bool b))

let rec convert_cps_vars (s : ivar list) (e : iast) : lamcom = 
  let list_posn (lst : 'a list) (item : 'a) : int option = 
    let rec list_posni (lst : 'a list) (item : 'a) (n : int) : int option = 
      match lst with
      | h::t when h = item -> Some n
      | h::t -> list_posni t item (n + 1)
      | [] -> None
    in 
    list_posni lst item 0
  in
  match e with 
  | App (e1, e2) -> App (convert_cps_vars s e1, convert_cps_vars s e2)
  | Lam (v, e') -> Lam (convert_cps_vars (v::s) e')
  | Bop (b, e1, e2) -> Bop (b, convert_cps_vars s e1, convert_cps_vars s e2)
  | Uop (u, e') -> Uop (u, convert_cps_vars s e')
  | If (g, e1, e2) -> If (convert_cps_vars s g,
                          convert_cps_vars s e1,
                          convert_cps_vars s e2)
  | Seq (e1, e2) -> failwith "Cannot directly convert a Seq"
  | Ref (e) -> failwith "Cannot directly convert a Ref"
  | While (e1, e2) -> failwith "Cannot directly convert a While"
  | Assign (e1, e2) -> failwith "Cannot directly convert a Assign"
  | Break -> failwith "Cannot directly convert a Break"
  | Continue -> failwith "Cannot directly convert a Continue"
  | Var v -> begin 
      match (list_posn s v) with 
      | Some i -> Var i
      | None -> failwith ("Unbound variable: " ^ v)
    end
  | Int i -> Int i
  | Bool b -> Bool b

(** [convert_cps e] converts an intermediate ast into a lambda expression in 
    continuation passing style.*)
let rec convert_cps (e : iast) : lamcom =
  e |> convert_cps_init |> convert_cps_vars []
