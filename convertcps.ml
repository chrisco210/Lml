open Lambdaast
open Iast



let counter = ref 0
(** [free_var] generates a fresh variable that has not been used before.  
    It will never match a user defined variable because it includes characters
    that cannot be included in user defined variable names. *)
let free_var () : ivar = 
  counter := !counter + 1; 
  "*/" ^ (string_of_int !counter) 


(** [convert_cps_init] This converts an iast into a iast in CPS style with 
    mutable slot passing. 

    These look like total gibberish so if you want to see the translations in 
    a readable "math" syntax, see TRANSLATION.md
*)
let rec convert_cps_init (e : iast) : iast = 
  match e with
  | App (e1, e2) -> 
    let k = free_var () in 
    let m = free_var () in 
    let k' = free_var () in 
    let m' = free_var () in 
    let k'' = free_var () in 
    let m'' = free_var () in 
    Lam (k,
         Lam (m,
              App (
                App (
                  (convert_cps_init e1),
                  (Lam (k', 
                        Lam (m',
                             App (
                               App (
                                 convert_cps_init e2, 
                                 Lam (k'',
                                      Lam (m'',
                                           App (
                                             App (
                                               App (Var k', Var k''), Var k
                                             ), Var m''
                                           )
                                          )
                                     )
                               ),
                               Var m'
                             )
                            )
                       ))
                ),
                Var m
              )
             )
        )
  | Lam (v, e') -> 
    let k = free_var () in 
    let m = free_var () in 
    let k' = free_var () in 
    let m' = free_var () in 
    Lam (k,
         Lam (m,
              App (
                App (
                  Var k, 
                  Lam (v, 
                       Lam(k',
                           Lam (m',
                                App (
                                  App (convert_cps_init e', Var k'),
                                  Var m'
                                )
                               )
                          )
                      )
                ),
                Var m
              )
             )
        )
  | Bop (b, e1, e2) -> 
    let k = free_var () in 
    let m = free_var () in 
    let k' = free_var () in 
    let m' = free_var () in 
    let k'' = free_var () in 
    let m'' = free_var () in 
    Lam (k,
         Lam (m,
              App (
                App (
                  (convert_cps_init e1),
                  (Lam (k', 
                        Lam (m',
                             App (
                               App (
                                 convert_cps_init e2, 
                                 Lam (k'',
                                      Lam (m'',
                                           App (
                                             App (
                                               Var k, (Bop (b, Var k', Var k''))
                                             ), Var m''
                                           )
                                          )
                                     )
                               ),
                               Var m'
                             )
                            )
                       ))
                ),
                Var m
              )
             )
        )
  | Uop (u, e') -> 
    let k = free_var () in 
    let m = free_var () in 
    let k' = free_var () in 
    let m' = free_var () in 
    Lam (k, 
         Lam (m,
              App (
                App (
                  (convert_cps_init e'),
                  (Lam (k',
                        Lam (m', 
                             App (
                               App (
                                 Var k,
                                 Uop (u, Var k')
                               ),
                               Var m'
                             )
                            )
                       ))
                ),
                Var m
              )
             )
        )
  | If (b, e1, e2) -> 
    let k = free_var () in 
    let m = free_var () in 
    let k' = free_var () in 
    let m' = free_var () in 
    let k'' = free_var () in 
    let m'' = free_var () in 
    Lam (k, 
         Lam (m, 
              App (
                App (
                  convert_cps_init b,
                  Lam (k', 
                       Lam (m',
                            If (Var k',
                                App (
                                  App (
                                    convert_cps_init e1,
                                    Lam (k'',
                                         Lam (m'',
                                              App (
                                                App (Var k, Var k''),
                                                Var m''
                                              )
                                             )
                                        )
                                  ),
                                  Var m'
                                ),
                                App (
                                  App (
                                    convert_cps_init e2,
                                    Lam (k'',
                                         Lam (m'',
                                              App (
                                                App (Var k, Var k''),
                                                Var m''
                                              )
                                             )
                                        )
                                  ),
                                  Var m'
                                )
                               )
                           )
                      )
                ),
                Var m
              )
             )
        )
  | Seq (e1, e2) -> 
    let k = free_var () in 
    let m = free_var () in 
    let k' = free_var () in 
    let m' = free_var () in 
    Lam (k,
         Lam (m,
              App (
                App (
                  convert_cps_init e1, 
                  Lam (k', 
                       Lam (m',
                            App (
                              App (
                                convert_cps_init e2,
                                Var k
                              ),
                              Var m'
                            )
                           )
                      )
                ), Var m
              )
             )
        )
  | Ref (e) -> failwith "Unimplemented convert_cps_init"
  | Deref (e) -> failwith "Unimplemented convert_cps_init"
  | Assign (e1, e2) -> failwith "Unimplemented convert_cps_init"
  | While (e1, e2) -> 
    let zcomb = 
      Lam ("**f", 
           App (
             (Lam ("**x", App (Var "**f", Lam ("**y", App (App (Var "**x", Var "**x"), Var "**y"))))), 
             (Lam ("**x", App (Var "**f", Lam ("**y", App (App (Var "**x", Var "**x"), Var "**y"))))) 
           )
          ) in 
    let k = free_var () in 
    let m = free_var () in 
    let f = free_var () in 
    let m' = free_var () in 
    let k'' = free_var () in 
    let m'' = free_var () in 
    let k''' = free_var () in 
    let m''' = free_var () in 
    Lam (k,
         Lam (m,
              App (
                App (
                  zcomb,
                  Lam (f,
                       Lam (m',
                            App(
                              App (
                                convert_cps_init e1,
                                Lam (k'',
                                     Lam (m'',
                                          If (
                                            (Var k''),
                                            (App (
                                                App (
                                                  convert_cps_init e2,
                                                  Lam (k''',
                                                       Lam (m''',
                                                            App (
                                                              Var f, 
                                                              Var m'''
                                                            )
                                                           )
                                                      )
                                                ),
                                                Var m''
                                              )
                                            ),
                                            App (
                                              App (Var k, Unit),
                                              Var m''
                                            )
                                          )
                                         )
                                    )
                              ),
                              Var m'
                            )
                           )
                      )
                ),
                Var m
              )
             )
        )
  | Break -> failwith "Unimplemented convert_cps_init"
  | Continue -> failwith "Unimplemented convert_cps_init"
  | Var v -> 
    let k = free_var () in 
    let m = free_var () in
    Lam (k, Lam (m, App (App (Var k, Var v), Var m)))
  | Int i -> 
    let k = free_var () in 
    let m = free_var () in
    Lam (k, Lam (m, App (App (Var k, Int i), Var m)))
  | Bool b -> 
    let k = free_var () in 
    let m = free_var () in
    Lam (k, Lam (m, App (App (Var k, Bool b), Var m)))
  | Unit -> 
    let k = free_var () in 
    let m = free_var () in
    Lam (k, Lam (m, App (App (Var k, Unit), Var m)))
  | Get -> 
    let k = free_var () in 
    let m = free_var () in 
    Lam (k, Lam (m, App (App (Var k, Var m), Var m)))
  | Set(e) ->
    let k = free_var () in
    let m = free_var () in
    let k' = free_var () in 
    let m' = free_var () in 
    Lam (k, 
         Lam (m, 
              App (
                App (
                  convert_cps_init e,
                  (
                    Lam (k', 
                         Lam (m', 
                              App (App (Var k, Unit), Var k')
                             )
                        )
                  )
                ),
                Var m
              )
             )
        )

(** [convert_cps_vars s e] converts an intermediate ast into a de bruijn AST*)
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
  | Deref (e) -> failwith "Cannot directly convert a deref"
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
  | Unit -> Unit
  | Get -> failwith "Cannot be directly converted"
  | Set _ -> failwith "Cannot be directly converted"

(** [convert_cps e] converts an intermediate ast into a lambda expression in 
    continuation passing style.*)
let rec convert_cps (e : iast) : lamcom =
  e |> convert_cps_init |> convert_cps_vars [] 
