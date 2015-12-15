

let rec pair_to_list = function
  | Nil -> []
  | Pair(e1,Nil) ->  [e1] 
  | Pair(e1, Pair (x,y) ) -> e1 ::pair_to_list (Pair (x,y))
  | _ -> raise (err "pair_to_list, last case")
;;


let contains_nested_defines bdy = 
  let rec helper = function
    | [] -> false
    | Pair(Symbol "define", _ ) :: _ -> true
    | Pair(Symbol "begin", bdy) :: xs ->
         if helper (pair_to_list bdy) then true
         else helper xs
    | x :: xs ->helper xs in
  helper bdy
;;


let partition_around_defines bdy = 
  (*we want to flatten the body of the list*)
  let rec helper defines bdy = match bdy with
    | [] -> (defines,bdy)
    |Pair(Symbol "define",dby)::xs ->
      helper (dby :: defines) xs
    | Symbol("begin") :: xs -> helper defines xs 
    | Pair(Symbol "begin",bbdy) :: xs -> 
        let sdefines,sbdy = helper defines (pair_to_list bbdy) in
        (match sbdy with
          | [] -> helper sdefines xs
          | x -> 
              (sdefines, (x@xs))
        )
    | _ -> (defines,bdy)
  in
  helper []  bdy
;;
(* a valid lambda is one s.t it's body gets partitioned into
 * (s,d) s.t contains_nested_defines d -> false*)

let transform_lambda_body bdy =  
  let create_ribs defines = 
    List.fold_right (fun a s -> Pair(a,s))
    defines
    Nil
    in
  let create_body tail =
    List.fold_right (fun a s -> Pair(a,s))

    tail
    Nil 
  in   
  let bdy' = pair_to_list bdy in 
  match (contains_nested_defines bdy') with
 | false -> bdy
 | true ->
     let defines,tail =partition_around_defines bdy' in
     (match contains_nested_defines tail with
      |false ->  
          let ribs,bdy = create_ribs (List.rev defines),create_body tail in
           Pair(Pair(Symbol "letrec", Pair(ribs,bdy)),Nil)
      |true -> raise (err "nested define in the middle of the body")
     )
;;


