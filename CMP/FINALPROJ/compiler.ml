(* compiler.ml
 * A compiler from Scheme to CISC
 *
 * Programmer: Mayer Goldberg, 2015
 *)

#use "pc.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
exception X_error of string;; 

let err str = X_error str;;
let debug str = Printf.printf "DEBUG: %s\n" str;;
let rec ormap f s =
  match s with
  | [] -> false
  | car :: cdr -> (f car) || (ormap f cdr);;

let rec andmap f s =
  match s with
  | [] -> true
  | car :: cdr -> (f car) && (andmap f cdr);;	  

let string_to_list str =
  let rec loop i limit =
    if i = limit then []
    else (String.get str i) :: (loop (i + 1) limit)
  in
  loop 0 (String.length str);;

let list_to_string s =
  let rec loop s n =
    match s with
    | [] -> String.make n '?'
    | car :: cdr ->
       let result = loop cdr (n + 1) in
       String.set result n car;
       result
  in
  loop s 0;;

type fraction = {numerator : int; denominator : int};;

type number =
  | Int of int
  | Fraction of fraction;;

type sexpr =
  | Void
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | Vector of sexpr list;;

module type SEXPR = sig
  val sexpr_to_string : sexpr -> string
end;; (* signature SEXPR *)

module Sexpr : SEXPR = struct
  
exception X_invalid_fraction of fraction;;

let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (Char.lowercase ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

let rec starts_with_paren = function
  | Pair(_,_) | Nil -> true
  | _ -> false
;;
  

let char_of_string = function
  | '\n' -> "newline"
  | '\r' -> "return"
  | '\t' -> "tab"
  | '\012' -> "page"
  | ' ' -> "space"
  | e -> list_to_string [e]

let rec sexpr_to_string = function
  | Void -> "void"
  | Bool e -> if e then "#t" else "#f" 
  | Nil -> "()"
  | Number e -> (match e with
            | Int x -> (string_of_int x)
            | Fraction {numerator=a;denominator=b} ->
                (string_of_int a) ^ "/" ^ (string_of_int b))

  | Char e ->"#\\" ^ char_of_string e 
  | String e ->  "\"" ^e ^ "\""
  | Symbol e -> e
  | Pair (e1,e2) ->  
      let e1 = "(" ^ (sexpr_to_string e1) ^ " " in
      let need_dot = starts_with_paren e2 in
      if need_dot then
        let str = sexpr_to_string e2 in
        let len = String.length str -2 in
        let str = String.sub str 1 len in
        e1 ^ str ^ ")"
      else
        e1 ^ "." ^ (sexpr_to_string e2) ^ ")"
  | Vector ls ->
      let bdy = List.map sexpr_to_string ls in
      let bdy = List.fold_right (fun a b -> a ^ " " ^ b)  
                bdy "" in
      "#(" ^ bdy ^ ")";;
            
end;; (* struct Sexpr *)

module type PARSER = sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list

end;;

module Parser : PARSER = struct

open PC;;


(* work on the tag parser starts here *)
(* ----------------------------------------------------------------------*)
(* -----------------------------------------------------------------------*)
(*Here we implement 3.1 : Comments & Whitespaces 
 * Here we have have line comments and sexpr comments, line blocks a line,
 *  sexpr a sexpr*)
let nt_sk_wh = 
  let nt =  nt_whitespace in
  pack nt (fun x -> Nil);;
let nt_line_comment =
  let nt_semi = char ';' in
  let nt_nline = disj (char '\n') (pack nt_end_of_input (fun _ -> '\n'))  in
  let nt = caten nt_semi (star (diff nt_any nt_nline)) in
  let nt = caten nt nt_nline in 
  pack nt (fun x -> Nil)
;;

(*Now we want to create a parser for le booleans, case insensitive*)
let nt_bool =
  let nt_true = pack (word_ci "#t") (fun _ -> Bool true) in
  let nt_false = pack (word_ci "#f") (fun _->Bool false) in
  let nt_bool = disj nt_true nt_false in
  nt_bool;;
(*Now we would like to make integers, we can just use the code from ass1*)

(*Mayer's nt_int*)

let make_char_value base_char displacement =
  let base_char_value = Char.code base_char in
  fun ch -> (Char.code ch) - base_char_value + displacement;;

let nt_digit_0_9 = pack (range '0' '9') (make_char_value '0' 0);;
  
let nt_digit_1_9 = pack (range '0' '9') (make_char_value '0' 0);;

let nt_int =
  let nt = char '-' in
  let nt = pack nt (fun e -> -1) in
  let nt' = char '+' in
  let nt' = pack nt' (fun e -> 1) in
  let nt = disj nt nt' in
  let nt = maybe nt in
  let nt = pack nt (function | None -> 1 | Some(mult) -> mult) in
  let nt' = range '0' '9' in
  let nt' = pack nt' (make_char_value '0' 0) in
  let nt' = plus nt' in
  let nt' = pack nt' (fun s -> List.fold_left (fun a b -> a * 10 + b) 0 s) in

  let nt = caten nt nt' in
  let nt = pack nt (fun (mult, n) -> (mult * n)) in
  nt;;
 
let nt_hex = 
  let nt_m = char '-' in
  let nt_m' = pack nt_m (fun e -> -1) in
  let nt_p = char '+' in
  let nt_p' = pack nt_p (fun e -> 1) in
  let nt_sign = disj nt_m' nt_p' in
  let nt_sign = maybe nt_sign in
  let nt_sign = pack nt_sign (function | None -> 1 | Some(mult) -> mult) in
  let nt_pref = word_ci "0x" in
  let nt_bdy = one_of_ci "abcdef" in
  let nt_nums = range '0' '9' in
  let nt = caten nt_pref (plus (disj nt_bdy nt_nums)) in
  let nt = pack nt (fun (_,ls) ->
                      "0x" ^ (list_to_string ls)) in
  let nt = pack nt int_of_string in
  let nt = caten nt_sign nt in
  let nt = pack nt (fun (mult, n) -> (mult * n)) in
  nt;;


(*END OF MAYER INT and NAT*)

let nt_scm_int =
  pack (disj nt_hex nt_int) (fun x-> Int x);;

let get_int_val = function
  | Int x -> x 
  | _ -> raise (err "get val defined only for ints");;


let rec gcd a b =
    if b = 0 then a else gcd b (a mod b);;

let nt_fract = 
  let nt_numer = nt_scm_int in
  let nt_slash = char '/' in 
  let nt_denom = nt_scm_int in
  pack (caten nt_numer (caten nt_slash nt_denom)) 
       (fun (a,(_,b)) -> 
          let a,b = get_int_val a, get_int_val b in
          if b < 1 
            then raise (err "X_invalid_fraction") 
          else
          let d = gcd a b in 
          let numerator = a/d in
          let denominator = b/d in
          if denominator = 1 then Int numerator else
          (Fraction {numerator ; denominator}));;
(*now we combine to create nt_number *)
let nt_number =
 let nt = pack (disj nt_fract nt_scm_int) (fun x -> Number x) in
 nt;;

(*Now we need to write a parser for le scheme symbols - 3.2.3*)
let nt_symbol =
  let nt_lowercase = pack (range 'A' 'Z') Char.lowercase in
  let nt_uppercase = range 'a' 'z' in
  let nt_digits = range '0' '9' in
  let nt_punctuation = disj_list[(char '!');(char '$');(char '^');
                              (char '^');(char '*');(char '-');
                              (char '_');(char '=');(char '+');
                              (char '<');(char '>');(char '/');
                              (char '?')] in
  let nt_part_of_symbol = disj_list[nt_lowercase;nt_uppercase;nt_digits;
                                     nt_punctuation] in
  let nt_body_of_symbol = plus nt_part_of_symbol in
  pack nt_body_of_symbol (fun b -> 
   try let (e,s) = nt_number b in 
      (match s with 
        | [] -> e 
        | _ -> 
            Symbol (list_to_string b) )
    with X_no_match -> Symbol(list_to_string b)) ;;


 
let nt_string =
  let nt_quote = char '"' in
  let meta_characters=[(1,(word "\\n"));(2,(word "\\r"));
                        (3,(word "\\t"));(4,(word "\\\\"));
                        (5,(word "\\\"")) ;(6, (word "\\f"))] in
  let meta_characters = List.map (function
    | (1,e) -> pack e (fun _ -> '\n')
    | (2,e) -> pack e (fun _ -> '\r')
    | (3,e) -> pack e (fun _ -> '\t')
    | (4,e) -> pack e (fun _ -> '\\')
    | (5,e) -> pack e (fun _ -> '\"')
    | (6,e) -> pack e (fun _ -> '\012')
    | _ -> raise (err "meta characters error")) meta_characters in
  let nt_meta = disj_list meta_characters in
  let nt_m_any = diff nt_any nt_meta in
  let nt_m_any = disj nt_m_any nt_meta in
  let nt_string =
    caten nt_quote(caten (star (diff nt_m_any nt_quote)) nt_quote)
     in
  pack nt_string (fun (_,(s,_)) ->String (list_to_string s));;
              

let nt_char = 
  let nt_prefix = word "#\\" in
  let foo word charval = pack (word_ci word) (fun _ -> Char charval) in
  let nt_named_chars = disj_list
                         [foo "newline" '\n';(foo "return" '\r');
                         (foo "tab" '\t');(foo "page" (Char.chr 12) );
                         (foo "space" ' ')] 
                       in
    
  let nt_visible_chars = const (fun x -> x > ' ') in
  let nt_visible_chars = pack nt_visible_chars (fun x -> Char x) in
  let nt_char = disj nt_named_chars nt_visible_chars in
  let nt_char = caten nt_prefix nt_char in
  pack nt_char (fun (_,b) -> b);;
    

(*nt_void*)

let nt_void = pack (word "void") (fun _ -> Void);;
let nt_s_comment nt_exp = 
  let nt_sexp_comment = caten (word "#;") nt_exp in 
  pack nt_sexp_comment (fun _ -> Nil ) ;;
let add_skip nt_s nt =
  let nt_s = nt_s_comment nt_s in     
  let nt_skip = star (disj_list [nt_sk_wh;nt_line_comment;nt_s]) in
  pack (caten nt_skip (caten nt nt_skip)) (fun (_,(b,_))->b) 


let ign = const (fun _ ->false);;
(* Now we have the recursive parsers left..  *)
let nt_sexpr =
  let rec make()=
    let nt_lb,nt_rb = add_skip ign (char '('), add_skip ign (char ')') in


    let rec nt_pair = 
     let nt_lb,nt_rb = add_skip (delayed make)  (char '('),
                        add_skip (delayed make) (char ')') in 
     let nt_sexpr = delayed make in
     let nt_dot = add_skip nt_sexpr (char '.') in
     let nt_proper_list = caten nt_lb (caten (star nt_sexpr) nt_rb) in
     let rec proper_to_pair = function
       | [] -> Nil
       | x::xs -> Pair(x,proper_to_pair xs) in
     let rec improper_to_pair = function
       | [] | [_] -> raise X_this_should_not_happen
       | x :: y :: [] -> Pair(x,y) 
       | x::xs -> Pair(x, improper_to_pair xs) in
     let nt_proper_list = pack nt_proper_list (fun (_,(ls,_)) -> ls) in
     let nt_proper_list = pack nt_proper_list proper_to_pair in
     let nt_improper_list = caten nt_lb (plus nt_sexpr) in
     let nt_improper_list = caten nt_improper_list nt_dot in
     let nt_improper_list = caten nt_improper_list (caten nt_sexpr nt_rb) in
     let nt_improper_list = pack nt_improper_list
                   (fun (((_,ls),_),(e,_)) -> ls @ [e]) in
     let nt_improper_list = pack nt_improper_list improper_to_pair in
     disj nt_improper_list nt_proper_list

    and nt_vector = 
     let nt_sexpr = star (delayed make) in
     let nt_start = word "#(" in
     let nt_vector = caten nt_start (caten nt_sexpr nt_rb) in
     let nt_vector = pack nt_vector (fun (_,(ls,_)) -> Vector ls) in
     nt_vector
    and nt_nil= 
      let nt_lb,nt_rb = add_skip (delayed make)  (char '('),
                        add_skip (delayed make) (char ')') in 
      let nt = caten nt_lb  nt_rb in
      let nt = caten (char '\'') nt in
      pack nt (fun _ -> Nil) 
    and nt_quote =
      let nt_sexpr = delayed make in
      let nt_unq_splice = pack  (word ",@") (fun _ -> 'q') in
      let nt_' = disj_list [add_skip nt_sexpr (char '\'');
                            add_skip nt_sexpr (char '`');
                            add_skip nt_sexpr nt_unq_splice 
                            ; add_skip nt_sexpr (char ',')] in
      let nt = caten nt_' nt_sexpr in
      pack nt (function
                | ('\'',sexpr) -> Pair(Symbol("quote"), Pair(sexpr,Nil)) 
                | ('`',sexpr) -> Pair(Symbol("quasiquote"), Pair(sexpr,Nil)) 
                | ('q',sexpr) ->
                    Pair(Symbol("unquote-splicing"),Pair(sexpr,Nil)) 
                | (',',sexpr) -> Pair(Symbol("unquote"), Pair(sexpr,Nil)) 
                | _ -> raise (err "in nt_quote"))

       

    in 
     let test = delayed make in
     let lst =  [nt_void;nt_bool;nt_nil;nt_symbol;
                nt_number;nt_string;
                nt_pair;nt_vector;nt_char;nt_quote] in
     disj_list (List.map (add_skip test) lst)  
  in
  make();;


let read_sexpr str =
  let str = string_to_list str in
  match (nt_sexpr str) with
  |(a,[]) -> a 
  | _ -> raise (err "did not fully parse") 

let read_sexprs string = 
  let rec read_exps acc charlst =
   let (e,s) = nt_sexpr charlst in 
   let acc = e :: acc in
   (match s with 
   | [] -> acc
   | _ ->read_exps acc s
  )
  in
  let charlst = (string_to_list string) in
  read_exps [] charlst;;

end;;

 
type expr =
  | Const of sexpr
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list)

exception X_syntax_error;;

module type TAG_PARSER = sig
  val read_expression : string -> expr
  val read_expressions : string -> expr list
  val expression_to_string : expr -> string
  val tag_parse : sexpr -> expr
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "do"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;  

let rec process_scheme_list s ret_nil ret_one ret_several =
  match s with
  | Nil -> ret_nil ()
  | (Pair(sexpr, sexprs)) ->
     process_scheme_list sexprs
			 (fun () -> ret_one sexpr)
			 (fun sexpr' -> ret_several [sexpr; sexpr'])
			 (fun sexprs -> ret_several (sexpr :: sexprs))
  | _ -> raise X_syntax_error;;

(*converts a Pair proper list into a list of sexpressions*)
let rec pair_to_list = function
  | Nil -> []
  | Pair(e1,Nil) ->  [e1] 
  | Pair(e1, Pair (x,y) ) -> e1 ::pair_to_list (Pair (x,y))
  | _ -> raise (err "pair_to_list, last case")
;;
(* Given an improper list (l1.end) we return the pair l1 where l1 is a
 * list of sexpresions and end is the last element in such list*)

let rec improper_to_list acc = function
  | Pair(e1,Pair(x,y)) -> improper_to_list (e1 :: acc) (Pair(x,y))
  | Pair(e1,e2) -> 
      let acc = e1 :: acc in
      (List.rev acc,e2) 
  | (Symbol _) as s -> ([],s)
  | _  -> raise (err "improper_to_list invoked on non pair");;

let scheme_list_to_ocaml_list args = 
  process_scheme_list args
		      (fun () -> [])
		      (fun sexpr -> [sexpr])
		      (fun sexprs -> sexprs);;
    
let expand_let_star ribs sexprs =
  let ribs = scheme_list_to_ocaml_list ribs in
  let params = List.map (function
			  | (Pair(name, (Pair(expr, Nil)))) -> name
			  | _ -> raise X_this_should_not_happen) ribs in
  let args = List.map
	       (function
		 | (Pair(name, (Pair(expr, Nil)))) -> expr
		 | _ -> raise X_this_should_not_happen) ribs in
  let params_set = List.fold_right
		     (fun a s ->
		      if (ormap
			    (fun b ->
			     (match (a, b) with
			      | (Symbol a, Symbol b) -> a = b
			      | _ -> raise X_this_should_not_happen))
			    s)
		      then s else a :: s)
		     params
		     [] in
  let place_holders = List.fold_right
			(fun a s -> Pair(a, s))
			(List.map
			   (fun var -> (Pair(var, (Pair((Bool false), Nil)))))
			   params_set)
			Nil in
  let assignments = List.map2
		      (fun var expr ->
		       (Pair((Symbol("set!")),
			     (Pair(var, (Pair(expr, Nil)))))))
		      params args in
  let body = List.fold_right
	       (fun a s -> Pair(a, s))
	       assignments
	       sexprs in
  (Pair((Symbol("let")), (Pair(place_holders, body))));;

let expand_letrec ribs sexprs =
  let ribs = scheme_list_to_ocaml_list ribs in
  let params = List.map (function
			  | (Pair(name, (Pair(expr, Nil)))) -> name
			  | _ -> raise X_this_should_not_happen) ribs in
  let args = List.map
	       (function
		 | (Pair(name, (Pair(expr, Nil)))) -> expr
		 | _ -> raise X_this_should_not_happen) ribs in
  let ribs = List.map
	       (function
		 | (Pair(name, (Pair(expr, Nil)))) ->
		    (Pair(name, (Pair(Bool false, Nil))))
		 | _ -> raise X_this_should_not_happen)
	       ribs in
  let body = List.fold_right
	       (fun a s -> Pair(a, s))
	       (List.map2
		  (fun var expr ->
		   (Pair((Symbol("set!")),
			 (Pair(var, (Pair(expr, Nil)))))))
		  params args)
	       sexprs in
  let ribs = List.fold_right
	       (fun a s -> Pair(a, s))
	       ribs
	       Nil in
  (Pair((Symbol("let")), (Pair(ribs, body))));;

exception X_unquote_splicing_here_makes_no_sense;;

let rec expand_qq sexpr = match sexpr with
  | (Pair((Symbol("unquote")), (Pair(sexpr, Nil)))) -> sexpr
  | (Pair((Symbol("unquote-splicing")), (Pair(sexpr, Nil)))) ->
     raise X_unquote_splicing_here_makes_no_sense
  | (Pair(a, b)) ->
     (match (a, b) with
      | ((Pair((Symbol("unquote-splicing")), (Pair(a, Nil)))), b) ->
	 let b = expand_qq b in
	 (Pair((Symbol("append")),
	       (Pair(a, (Pair(b, Nil))))))
      | (a, (Pair((Symbol("unquote-splicing")), (Pair(b, Nil))))) ->
	 let a = expand_qq a in
	 (Pair((Symbol("cons")), (Pair(a, (Pair(b, Nil))))))
      | (a, b) ->
	 let a = expand_qq a in
	 let b = expand_qq b in
	 (Pair((Symbol("cons")), (Pair(a, (Pair(b, Nil)))))))
  | (Vector(sexprs)) ->
     let s = expand_qq (List.fold_right (fun a b -> Pair(a, b)) sexprs Nil) in
     (Pair((Symbol("list->vector")), (Pair(s, Nil))))
  | Nil | Symbol _ -> (Pair((Symbol("quote")), (Pair(sexpr, Nil))))
  | expr -> expr;;




let create_body f = function
  | [] -> Const Void
  | x::[] -> f x
  | xs -> Seq (List.map f xs)


let rec expand_cond = function 
  | [] ->raise  (err "expanding empty cond")
  | [Pair(Symbol "else",e1)] -> Pair(Symbol "begin",e1)
  | [Pair(test,e1)] -> 
     Pair (Symbol "if", Pair (test, Pair (Pair(Symbol "begin", e1), Nil))) 
  | Pair(test,Pair(e1,Nil)) :: xs ->
    Pair(Symbol "if", Pair (test, Pair (e1,Pair((expand_cond xs), Nil))))
  | Pair(test,es) :: xs ->
    Pair(Symbol "if", Pair(test,
     Pair(Pair(Symbol "begin",es),Pair((expand_cond xs),Nil))))  
  | _ -> raise (err "Probably not a cond expression ;(")
 



(* we map f on the args *)
let expand_let args bdy f =
  let args = pair_to_list args in
  let args_vals = List.map pair_to_list args in 
  let params = List.map (function 
                         | [Symbol x ; _]  -> x
                         | _ -> raise (err "error let expand. in map"))
                       args_vals in
  let args = List.map (fun ls -> List.nth ls 1) args_vals in
  let bdy = create_body f (pair_to_list bdy) in
  let lambda = LambdaSimple(params,  bdy) in
  Applic (lambda, (List.map f args));; 


 
let rec expand_and = function
  | [] -> Bool true
  | x :: [] -> x
  | x :: y ::  []  -> 
     Pair (Symbol "if", Pair (x, Pair (y, Pair (Bool false, Nil))))  
  | x :: xs ->
      Pair (Symbol "if", Pair (x, Pair (expand_and xs,
       Pair (Bool false, Nil))))  
 (*checks if a given pair , is a proper list*)
let rec is_proper = function
  | Pair(_,Nil)  | Nil-> true
  | Pair(_,Pair(x,y)) -> is_proper (Pair (x,y))
  | _ -> false;;


let get_sym = (function |Symbol str -> str 
                         |_->raise (err "Non symbol in lambda args")) ;; 

let make_var v =
  if List.mem v reserved_word_list then
    raise X_syntax_error
  else
    Var v;;
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
    | Pair (Symbol "define", Pair (Pair (func, ls),bdy)) :: xs ->
        let expr = Pair(Symbol "lambda",Pair (ls,bdy)) in
        let bdy = Pair((Symbol "define", Pair(func, Pair(expr,Nil))))
          :: xs in
         helper defines bdy
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
    
 
let rec tag_parse = function
  (*first couple are the cases, where we have plain Consts *)
  |(Char _) as c -> Const c   | (Number _) as n -> Const n
  |(Bool _) as b ->Const b    | Nil -> Const Nil
  |(String _) as s -> Const s | Void -> Const Void 

  (*dealing with quotes*)
  |Pair (Symbol "quote", Pair(e,Nil))  -> Const e 
  |(Pair (Symbol "unquote", Pair(e,Nil))) -> raise X_syntax_error 
  |Pair (Symbol "quasiquote", Pair(e,Nil)) -> tag_parse (expand_qq e) 
 
(*Dealing with defines*)
  (*what happens if we have a list of expressions in the body...?*) 

  |Pair (Symbol "define", Pair (Pair (Symbol func, ls),bdy))  ->
      let expr = tag_parse (Pair (Symbol "lambda", Pair (ls, bdy))) in
      Def (make_var func,expr)


  |(Pair((Symbol("define")), (Pair((Symbol(var)),
   (Pair( exp, Nil)))))) -> Def ( make_var var, tag_parse exp)
  (*need to check var against list of reserved words*)




  |(Pair((Symbol("if")), (Pair(test, (Pair(dit, (Pair(dif, Nil)))))))) ->
       If (tag_parse test, tag_parse dit, tag_parse dif)
  |(Pair((Symbol("if")), (Pair(test, (Pair(dit, Nil)))))) ->
      If(tag_parse test,tag_parse dit, tag_parse Void)



  |Pair (Symbol "lambda", Pair (ls, bdy)) -> 
      let bdy = transform_lambda_body bdy in
      let bdy = create_body tag_parse (pair_to_list bdy) in
      if is_proper ls then 
        let args = pair_to_list ls in
        let args = List.map get_sym args in
        LambdaSimple (args, bdy)
      else
        let (args,variadic) = improper_to_list [] ls in
        let args = List.map get_sym args in
        let variadic = get_sym variadic in
        LambdaOpt (args, variadic, bdy)

  (*Or expression*)
  |Pair (Symbol "or", ls) ->
      if ls = Nil then Const (Bool false) else
      let exprls = List.map tag_parse (pair_to_list ls) in
      (match exprls with
      | [] -> raise (err "empty list in or") 
      | [x] -> x
      | _ -> Or exprls
      )
  (*cond shiz*)
  |Pair (Symbol "cond", ls) -> tag_parse (expand_cond (pair_to_list ls))

  |Pair (Symbol "and", ls) -> 
      let ls = if ls = Nil then [] else (pair_to_list ls) in
      tag_parse (expand_and ls)
    
     (*dealign with let *)

  |Pair (Symbol "let", Pair (ls, bdy))  -> 
       expand_let ls bdy tag_parse 

  (*dealing with let star *)
  |Pair (Symbol "let*", Pair (ls, bdy))  -> 
      tag_parse (expand_let_star ls bdy)

    (*dealing with let rec *)
  |Pair (Symbol "letrec", Pair (ls, bdy))  -> 
      tag_parse (expand_letrec ls bdy)


  |Pair (Symbol "set!", Pair(Symbol var,exp)) ->
      let var = make_var var in
      let bdy = create_body tag_parse (pair_to_list exp) in
      Set(var,bdy)
  |Pair (Symbol "begin", bdy) -> 
      create_body tag_parse (pair_to_list bdy) 
  (*Application shit*)
  |Pair ( func , args) -> 
      Applic (tag_parse func,
            List.map tag_parse (pair_to_list args))
  |Symbol s -> make_var s
  |Vector v -> raise X_syntax_error
    
;;
let read_expression string = tag_parse (Parser.read_sexpr string);;

let read_expressions string =
  List.rev( List.map tag_parse (Parser.read_sexprs string));;

let cat_space a b = a ^ " " ^ b;;


let rec expression_to_string = function 
  |Const Void -> ""
  |Const Nil -> "'()"
  |Const ((Pair (_,_)) as a) | Const ((Symbol _) as a)
             -> "'" ^ Sexpr.sexpr_to_string a
  |Const a -> Sexpr.sexpr_to_string a
  |Var s -> s
  |If (test,dit,dif) ->
      let test,dit,dif = expression_to_string test, expression_to_string dit
                 , expression_to_string dif in
      "(if " ^ test ^ " " ^ dit ^ " " ^ dif ^ ")"
  |Seq es -> 
    let strs = List.map expression_to_string es in
    (match strs with
     | [] ->  expression_to_string (Const Void)
     | x::[]-> List.hd strs
     | _ ->
    "(begin " ^(List.fold_right cat_space strs ")") 
    )
  |Set (e1,e2) -> 
    let e1,e2 = expression_to_string e1, expression_to_string e2 in
    "(set! " ^ e1 ^ " " ^ e2 ^ ")"
  |Def (e1,e2) ->
    let e1,e2 = expression_to_string e1, expression_to_string e2 in
    "(define " ^ e1  ^ " " ^ e2 ^ ")"
  |Or es -> 
    let strs = List.map expression_to_string es in
    "(or " ^ (List.fold_right cat_space strs "") ^ ")"
  |LambdaSimple (params,bdy) ->   
    let params,bdy=List.fold_right cat_space params "",
                       expression_to_string bdy in
    "(lambda (" ^ params ^") " ^ bdy ^")"
  |LambdaOpt ([], v,bdy) ->
     "(lambda " ^ v ^ " " ^ (expression_to_string bdy) ^ ")"
  |LambdaOpt (params,v,bdy) -> 
    let params,bdy=List.fold_right cat_space params "",
                 expression_to_string bdy in
    let params = params ^ " . " ^ v in
    "(lambda (" ^ params ^") " ^ bdy ^")"
  |Applic (op, args) ->
      let op,args = expression_to_string op,
                    List.map expression_to_string args in
      let args = List.fold_right cat_space args "" in
      "(" ^ op ^ " " ^ args ^ ")"
;;
 

end;; (* struct Tag_Parser *)

let test_parser string =
  let expr = Tag_Parser.read_expression string in
  let string' = (Tag_Parser.expression_to_string expr) in
  Printf.printf "%s\n" string';;



type var = 
  | VarFree' of string
  | VarParam' of string * int
  | VarBound' of string * int * int;;

type expr' =
  | Const' of sexpr
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;



module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end
;;

module Semantics : SEMANTICS = struct
let range b =
  let rec helper a = 
    if a = b then [] 
    else
      a :: (helper (a+1)) in
  helper 0;;
  
let create_assoc lst =
  List.map2 (fun a b -> (a,b)) lst (range (List.length lst))
;; 
let annotate_lexical_addresses e = 
  let rec run pvars bvars = function
    | Const e -> Const' e
    | If(test,dit,dif)->
        If'(run pvars bvars test,
            run pvars bvars dit,
            run pvars bvars dif)
    | Seq es->
        Seq' (List.map (run pvars bvars) es) 
    | Or es ->
        Or' (List.map (run pvars bvars) es)
    | Def (Var v,e) -> Def' (Var' (VarFree' v) , 
                              run pvars bvars e)
    | Set(v,e) -> Set' (run pvars bvars v, run pvars bvars e)
    | LambdaSimple(strs,e) ->
        let strs' = create_assoc strs in 
        LambdaSimple'(strs, run strs' (pvars::bvars) e)
    | LambdaOpt(strs,str,e) ->
        let strs' = strs @ [str] in
        let strs' = create_assoc strs' in
        LambdaOpt' (strs,str, run strs' (pvars::bvars) e)
    | Applic (func, args) ->
        Applic' (run pvars bvars func, 
                 List.map (run pvars bvars) args)
    | Var e ->
        let rec helper index =( function
          | [] -> Var' (VarFree' e) 
          | x ::xs -> if List.mem_assoc e x 
                      then
                        Var' (VarBound' (e,index,List.assoc e x))
                      else 
                        helper (index+1) xs
                       )  in
        if List.mem_assoc e pvars then
          Var' (VarParam' (e, List.assoc e pvars))
        else 
          helper 0 bvars
    | _ -> raise (err "missed a case in run wtf")     
  in              
  run [] [] e

;;



let split = function
  | [] -> raise (err "you cannot under any circumstances split an empty list")
  | xs -> 
      let xs' = List.rev xs in
      (List.rev (List.tl xs'), List.hd xs') 
;;
let annotate_tail_calls e = 
  let rec run intail = function
    | (Const' _) as e -> e | (Var' _) as e -> e
    | (Box' _) as e -> e | (BoxGet' _ ) as e -> e
    | (BoxSet' _) as e -> e 
    | Def' (v,e)  -> Def' (v, run false e)
    | Set' (v,e) -> Set' (v, run intail e)
    | If'(test,dit,dif) ->
        If'(run false test, run intail dit, run intail dif)
    | Or' exprs -> 
        let xs,last = split exprs in
        Or' ((List.map (run false) xs) @ [run intail last])
    | Seq' exprs ->
        let xs,last = split exprs in
        Seq' ((List.map (run false) xs) @ [run intail last])
    | LambdaSimple' (params,bdy) ->
        LambdaSimple'(params, run true bdy)
    | LambdaOpt' (params,param,bdy) ->
        LambdaOpt' (params,param, run true bdy)
    | Applic' (func,args) ->
        let func,args = (run false func, List.map (run false) args) in
        if intail then 
          ApplicTP' (func,args) 
        else 
          Applic' (func,args) 
    | ApplicTP' _ ->
        raise (err "How, in heavens name, did we manage to get here")




  in

  run false e;;


let create_body'  = function
  | [] -> Const' Void
  | x::[] -> x
  | xs -> Seq' xs



let get_var = function 
  |VarFree' s -> s
  |VarParam' (s,_) -> s
  |VarBound' (s,_,_) -> s 
;;  
let box_set e =
  let rec need_box var =
    function 
    | Const' _ | Var' _ | Box' _ | BoxGet' _ | BoxSet' _ | Def' _ -> false
    | If'(test,dit,dif) ->
        ( (need_box var) test) ||
        ( (need_box var) dit) ||
        ( (need_box var) dif)
    | Or' exprs -> ormap  (need_box var) exprs 
    | Seq' exprs -> ormap  (need_box var) exprs
    | LambdaSimple'(params,bdy) -> 
        if List.mem var params then false
        else  (need_box var) bdy
    | LambdaOpt' (params,param,bdy) -> (*not sure what to do amigo*)
        if List.mem var (param::params) then false
          else  (need_box var) bdy 
    | Applic'(func,args) | ApplicTP'(func,args) ->
        (need_box var) func || ormap  (need_box var) args 
    | Set' (Var' v,e) ->
        if (get_var v) = var then true
        else need_box var e
    | Set' (_,_) -> raise (err "non variable in car of Set'")
  in
  let rec get_occur var = function
    | Var' (VarParam' (v,_))->var=v 
    | Var'(VarBound' (v,_,_))->var=v 
    | If'(test,dit,dif) -> (get_occur var) test || (get_occur var) dit ||
                           (get_occur var) dif
    | Or' exprs | Seq' exprs -> ormap (get_occur var) exprs 
    | LambdaSimple'(params,bdy) ->
        if List.mem var params then false
        else get_occur var bdy
    | LambdaOpt' (params,param,bdy) ->
        if List.mem var (param::params) then false
        else get_occur var bdy
    | Applic'(func,args) | ApplicTP'(func,args) -> 
        get_occur var func || ormap (get_occur var) args
    | Set'(_,e) -> get_occur var e
    | _ -> false
    
  in 
  let rec bound_occur var = function
    | Var' (VarParam' (v,_)) ->false 
    | Var'(VarBound' (v,_,_)) ->var=v 
    | If'(test,dit,dif) -> (bound_occur var) test || (bound_occur var) dit ||
                           (bound_occur var) dif
    | Or' exprs | Seq' exprs -> ormap (bound_occur var) exprs 
    | LambdaSimple'(params,bdy) ->
        if List.mem var params then false
        else bound_occur var bdy
    | LambdaOpt' (params,param,bdy) ->
        if List.mem var (param::params) then false
        else bound_occur var bdy
    | Applic'(func,args) | ApplicTP'(func,args) -> 
        bound_occur var func || ormap (bound_occur var) args
    | Set'(v ,e) -> bound_occur var v ||  bound_occur var e
    | _ -> false 
  in
  let get_prefix params'  = 
    let param_indexes = create_assoc params' in
     List.map (fun (a,b) -> 
         Set' (Var' (VarParam' (a,b)),Box' (VarParam' (a,b)))) param_indexes 
  in
  let needbox bdy = 
   (fun x->(need_box x bdy)&&(get_occur x bdy)&&(bound_occur x bdy))         
  in
    
  let rec run to_change = function 
    | (Var' (VarParam' (v,n))) as e ->
        if List.mem v to_change then
          BoxGet' (VarParam' (v,n)) 
        else e
    | (Var' (VarBound' (v,m1,m2))) as e ->
        if List.mem v to_change then
          BoxGet' (VarBound' (v,m1,m2))
        else e
    | Set' (Var' x, e) -> 
        (match x with
          | VarBound' (v,m1,m2) -> 
              if List.mem v to_change 
                then BoxSet' (VarBound' (v,m1,m2) , run to_change e)
                else Set'(Var' x, run to_change e)
          | VarParam' (v,n) -> 
              if List.mem v to_change
              then 
                BoxSet' (VarParam'(v,n), run to_change e)
              else Set'(Var' x, run to_change e)
          | _ -> Set'(Var' x, run to_change e)
)
    |Set' _ -> raise (err ("set with non var car"))            
    
    | (Const' _) as e -> e | (Var' _) as e -> e
    | (Box' _) as e -> e | (BoxGet' _ ) as e -> e
    | (BoxSet' _) as e -> e 
    | Def' (v,e)  -> Def'(v, run to_change e)
    | If' (test,dit,dif) ->
        If' (run to_change test,
             run to_change dit,
             run to_change dif)
    | Seq' exprs ->
        Seq' (List.map (run to_change) exprs)
    | Or' exprs ->
        Or' (List.map (run to_change) exprs)
    | Applic' (func,args) -> 
        Applic' (run to_change func, (List.map (run to_change) args))
    | ApplicTP' (func,args) ->
        ApplicTP'(run to_change func, (List.map (run to_change) args))
    | LambdaSimple' (params,bdy) ->
        let to_change = 
          List.filter (fun x -> not (List.mem x params)) to_change in
        let params' = List.filter 
          (needbox bdy) params in
        let prefix = get_prefix params' in
        let bdy =   run (params' @ to_change) bdy in
        let bdy = create_body' (prefix @ [bdy]) in
        LambdaSimple'(params, bdy)
    | LambdaOpt' (params,param,bdy) ->
        let params' = param :: params in
        let to_change =
          List.filter (fun x -> not (List.mem x params')) to_change in
        let params'' = List.filter 
          (needbox bdy)  params' in
        let prefix= get_prefix params'' in
        let bdy =   run (params'' @ to_change) bdy in
        let bdy = create_body' (prefix @ [bdy]) in
        LambdaOpt'(params,param,bdy)
  in run [] e


;;
let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)

module IO = struct

  let file_to_string input_file =
  let in_channel = open_in input_file in
  let rec run () =
    try 
      let ch = input_char in_channel in ch :: (run ())
    with End_of_file ->
      ( close_in in_channel;
        [] )
  in list_to_string (run ());;

let string_to_file out_string output_file =
  let out_channel = open_out output_file in
  ( output_string out_channel out_string;
    close_out out_channel );;


end;;
module Constants = struct 
  let t_void=		937610;;
  let t_nil =	722689;;
  let t_bool= 		741553;;
  let t_char =		181048;;
  let t_integer= 	945311;;
  let t_string =	799345;;
  let t_symbol =	368031;;
  let t_pair 	=	885397;;
  let t_vector =	335728;;
  let t_closure =	276405;;
  let t_rational = 235937;;
  let const_tble = 2;;
  let string_list = const_tble-1;;
  let init= [(Void,[t_void]);
             (Nil,[t_nil]);
             (Bool false,[t_bool; 0]);
             (Bool true,[t_bool; 1]);];;



  let rec find expr  = function 
    |[] -> raise (err "something is wrong with the topilogical sort")
    |(a,b,c)::xs -> if a=expr then b else find expr xs;;
  let tble = ref [] ;;
  let table() = !tble;;
  let create_table lst = 
    let rec helper so_far curr_add= function
      | [] -> so_far
      |Pair(a,b) :: rest -> 
          let tuple = (Pair(a,b),
                       curr_add,
                       [t_pair;find a so_far; find b so_far]) in
          helper (tuple::so_far) (curr_add+3) rest 
      |Vector exprs :: rest ->
        let lst = List.map (fun x->find x so_far) exprs in
        let lst = t_vector :: List.length lst :: lst in
        let tuple = (Vector exprs, curr_add, lst) in
        helper (tuple ::so_far) (curr_add + List.length lst) rest
      |Bool e :: rest-> 
          let tuple =
            (Bool e, curr_add,[t_bool;if e then 1 else 0]) in
          helper (tuple::so_far) (curr_add+2) rest
      |Number (Int n) :: rest -> 
          let tuple = (Number(Int n),curr_add,[t_integer;n]) in
          helper (tuple::so_far) (curr_add+2) rest
      (*|Number x ->raise X_not_yet_implemented*)
      |Char c :: rest ->
        let tuple = (Char c,curr_add,[t_char; Char.code c]) in
        helper (tuple::so_far) (curr_add+2) rest
      |String str :: rest ->
        let chars_codes=List.map Char.code (string_to_list str) in
        let chars_codes = t_string:: List.length chars_codes :: chars_codes in
        let tuple = (String str,curr_add,  chars_codes) in
        helper (tuple::so_far) (curr_add+List.length chars_codes) rest
      |Symbol str :: rest ->
        let in_mem = [t_symbol; find (String str) so_far] in
        let tuple = (Symbol str,curr_add, in_mem) in
        helper (tuple::so_far) (curr_add+List.length in_mem) rest


      |e::rest -> 
          let a,b = if e = Nil then (Nil,t_nil) else (Void,t_void) in
          let tuple = (a,curr_add,[b]) in
          helper (tuple::so_far) (curr_add+1) rest
       in
   tble := List.rev( helper [] const_tble lst)
  ;;
  let addr_table () =
    let tble = List.map (fun(_,_,z) -> z) (table()) in
    let tble = List.flatten tble in
    tble;;

 let get_const_prims () =
  let adds =  List.map
   (fun (a,_) -> string_of_int (find a !tble)) init in
  "#define VOID " ^ List.nth adds 0 ^ "\n" ^
  "#define NIL " ^ List.nth adds 1 ^ "\n" ^
  "#define FALSE " ^ List.nth adds 2 ^ "\n" ^
  "#define TRUE  " ^ List.nth adds 3 ^ "\n"    
;;
  let reset_const_tbl() =
    tble := [];;
  let create_string_list()=
    let const_tbl=table() in
    let symbols=List.filter (fun(_,_,c)->List.hd c=t_symbol) const_tbl in 
    if symbols <> [] then
      let string_addresses = List.map (fun (_,_,c) -> List.nth c 1) symbols in
      let mov_str b_add =         
          "MOV(INDD(R0,0), IMM("^b_add^")); \n"^
          "MOV(R1,R0); \n"^
          "ADD(R0,IMM(2)); \n" in
      let init_str = 
      "MOV(IND("^string_of_int string_list^"),R0); \n"^ 
      mov_str (string_of_int (List.hd string_addresses)) in         
      List.fold_left
        (fun a b->
          let b_add = string_of_int b in
            a^
            "MOV(INDD(R1,1),R0); \n"^
            mov_str b_add)
        init_str (List.tl string_addresses) , List.length symbols 
     else "",0         
;;
end;;
module Global_Env = struct
  let global = ref [];;
  let get_env () = !global;;
  let prims = (*cons car cdr +*)
    ["car";"cdr";"cons";"plus";
    "minus";"is_zero";"is_null";
    "mul";"is_list";"is_pair";
    "v_plus";"v_minus";"v_mult";
    "v_div";"vector";"apply";"make_string"];;
  let create_global_env lst =
    let rec helper curr_add acc = function
      |[] ->acc
      |Def'(Var' (VarFree' s), _)::rest->
          helper (curr_add+1) ((s,curr_add)::acc) rest
      |_::rest->helper curr_add acc rest in
    let off_set=1+Constants.const_tble+List.length (Constants.addr_table()) in
    let prims = List.mapi (fun a b -> (b, off_set+a)) prims in
    let off_set = off_set +List.length prims in
    let env = prims @ List.rev(helper off_set !global lst) in

    global :=env;
    env;;
  let foo () =
    let env = get_env() in
    let closure_creation = 
    List.fold_left 
      (fun a b -> 
        let b_add = string_of_int (List.assoc b env) in
        let s =
          "MOV(IND("^b_add^"),R0);\n" ^ 
          "MOV(IND(R0), IMM(T_CLOSURE)); \n" ^
          "INCR(R0);\n" ^
          "MOV(IND(R0),IMM(2131));\n" ^
          "INCR(R0); \n" ^
          "MOV(IND(R0), LABEL(L_"^b^"));\n"^
          "INCR(R0); \n" in 

        a  ^  s)
           "/* STARTING TO ADD PRIMITIVES */ \n" prims 
        in
        closure_creation;;
        (*
    let closure_prefix =
      List.fold_left 
      (fun a b -> 
        let b_add = string_of_int (List.assoc b env) in
        let s =
          "/* making " ^ b^" */ \n" ^ 
          "MOV(IND("^b_add^"),R0);\n" ^  
          "INCR(R0);\n" in 

        a  ^  s) "\n\n/* dummy-values for prims */ \n " prims in
        closure_prefix ^ closure_creation *)
    (*resets the global env, should be done after each compilation of a file*)      
  let reset_env () = 
    global:= [] 
  ;;


end;;

module type CODE_GEN = sig
  val code_gen: expr' -> string
  val compile_scheme_file: string -> string -> unit
end
;;
module Code_Gen : CODE_GEN = struct

let asm_comment s = 
  "/* " ^ s ^ "*/\n"
let label_counts = ref [];;

let gen_label s = 
  if List.mem_assoc s !label_counts then () 
     else label_counts:= (s,ref 0) :: !label_counts ;
  let count = List.assoc s !label_counts in
  let ans = s ^ (string_of_int !count) in
  count := !count +1;
  ans;; 


  
let start_of_function,end_function  = 
  "PUSH(FP);\nMOV(FP,SP);\n","POP(FP);\nRETURN;\n";;
let start_of_lambda label labelexit env_sz = 
   "// IN LAMBDA AAAAA \n"^
   "MOV(R1,1+"^env_sz^"); \n"^
   "PUSH(R1); \n" ^
   "CALL(MALLOC);\n"^
   "MOV(R1,R0); \nDROP(1); \n" ^
   "MOV(R2, FPARG(0)); \n" ^
   "for(int i=0,j=1; i < " ^ env_sz ^"; ++i, ++j){
     MOV(R3, INDD(R2,i));
     MOV(INDD(R1,j),R3);
   }\n"^
   "PUSH(FPARG(1)); \n" ^
   "CALL(MALLOC); \n" ^
   "DROP(1); \n" ^
   "for(int i=0; i<FPARG(1); ++i){
     MOV(R3,FPARG(2+i));
     MOV(INDD(R0,i),R3);
    }\n" ^
   "MOV(INDD(R1,0),R0); \n" ^
   "PUSH(IMM(3)); \n" ^
   "CALL(MALLOC); \n" ^
   "DROP(1); \n" ^
   "MOV(INDD(R0,0),T_CLOSURE);\n"^
   "MOV(INDD(R0,1),R1); \n"^
   "MOV(INDD(R0,2),LABEL("^label^"));\n" ^
   "JUMP(" ^labelexit ^"); \n"^
   label^":\n"^
     start_of_function 
;;
(*p - number of normal args in lambda_op*)
let lambda_op_fix p = 
  let p = string_of_int p in
  let loop_label,fin_label=
    gen_label "lambda_opt_loop", gen_label "finish_lambda_opt" in
  let stck_fix, fin_sck =gen_label "stack_fix" ,gen_label "stack_fix_end"in
  let stck_fix2, stck_fix3 = gen_label "empty_opt_case",gen_label "empty_opt"in
  let fin_sck2 = gen_label "end_of_nil_case_opt" in
  "MOV(R1,FPARG(1)); \n"^
  "MOV(R2,NIL); \n" ^ 
  loop_label ^ ":\n" ^
  " CMP(R1,"^p^");\n" ^
  " JUMP_EQ("^fin_label^"); \n" ^
  " PUSH(R2); \n" ^
  " PUSH(FPARG(R1+1)); \n" ^
  " CALL(MAKE_SOB_PAIR);\n" ^
  " DROP(2); \n"^
  " SUB(R1,1); \n"^
  " MOV(R2,R0); \n" ^
  " JUMP("^loop_label^");\n" ^
  fin_label ^": \n"^
  "CMP(IND(R2),T_NIL); \n" ^
  "JUMP_EQ("^stck_fix2^"); \n" ^
  "MOV(FPARG(2+"^p^"),R2); \n"^
  "MOV(R4,FP); \n "^
  "SUB(R4,IMM(4)); \n" ^
  "SUB(R4,FPARG(1)); // now r4 should hold number of old args \n"^
  "MOV(FPARG(1),1+"^p^");\n"^
  "MOV(R3,FP); // now r3 should hold new number of args \n" ^
  "SUB(R3,IMM(4)); \n" ^
  "SUB(R3,FPARG(1)); \n" ^
  "MOV(R5,R3); \n" ^
  "SUB(R5,R4); \n" ^
  "JUMP_EQ("^stck_fix2^"); \n"^
   stck_fix ^ ": \n" ^
  " CMP(R3,SP); \n" ^
  " JUMP_EQ("^fin_sck ^"); \n" ^
  " MOV(STACK(R4),STACK(R3)); \n" ^
  " INCR(R4); \n" ^
  " INCR(R3); \n" ^
  " JUMP(" ^ stck_fix ^ "); \n" ^
  stck_fix2^":\n " ^
  " MOV(R5,IMM(-1)); \n" ^
  " MOV(R6,SP);\n " ^
  " MOV(R3,SP); \n" ^
  " DECR(R3); \n " ^
  " MOV(R4,FP); \n "^
  " SUB(R4,IMM(4)); \n" ^
  " SUB(R4,FPARG(1)); // now r4 should hold number of old args \n"^
  stck_fix3 ^ ":\n" ^
  " CMP(R3,R4); \n" ^
  " JUMP_LT("^fin_sck2^"); \n" ^
  " MOV(STACK(R6),STACK(R3)); \n" ^ 
  " DECR(R6); \n" ^
  " DECR(R3); \n" ^
  " JUMP("^stck_fix3^");\n" ^
  fin_sck2 ^": \n" ^
  "MOV(STACK(R6),R2); \n" ^
  fin_sck ^": \n" ^
  "//need to fix the stack pointer stuff now \n"^
  "SUB(SP,R5); \n"^
  "MOV(FP,SP); \n" 

let push_imm n = "PUSH(IMM(" ^ n ^ ")); \n";;


let applic_bdy f argl proc= 
  let gen_args = List.map f (List.rev argl) in
  let num_args = string_of_int (List.length argl) in
  let fst_arg, gen_argtl =
        if gen_args <> [] then  (List.hd gen_args ,List.tl gen_args)
        else ("",[]) in 
  let sfx = if fst_arg = "" then "" else "PUSH(R0); \n" in
  let args_prog = 
    List.fold_left 
      (fun a b->
        a ^ "PUSH(R0); \n" ^ b) fst_arg gen_argtl ^ sfx in
  let prog = args_prog ^ push_imm num_args in
  let proc = f proc in
  let prog = "//IN APPLIC \n" ^ prog ^ proc in
  prog ^
  "PUSH(R0);\n"^
  "CALL(IS_SOB_CLOSURE); \n" ^
  "CMP(R0,0); \n"^
  "JUMP_EQ(L_ERROR_NOT_CLOSURE);\n" ^
  "POP(R0); \n" ^
  "PUSH(INDD(R0,1));\n"
;;

let normal_applic_suffix = 
      "CALLA(INDD(R0,2));\n" ^
      "POP(R1); \n" ^
      "POP(R1); \n" ^
      "DROP(R1); \n //OUT OF APPLIC \n" 
;;


let applic_tp_suffix arg_sz  =
  let lbl,lbl_jmp = gen_label "tail_call_copy", gen_label "jmpa" in 
  "PUSH(FPARG(-1));\n" ^  
  "MOV(R2,FP); \n" ^
  "SUB(R2,(FPARG(1)+4)); \n" ^
  "MOV(R3,FP); \n" ^
  "MOV(FP,FPARG(-2));\n" ^
  "MOV(R5,R3+"^arg_sz^"+3); \n" ^
  lbl ^":\n"^
  "CMP(R3,R5); \n"^
  "JUMP_EQ("^lbl_jmp^"); \n"^
  " MOV(STACK(R2),STACK(R3)); \n"^
  " ADD(R3,1); \n" ^
  " ADD(R2,1); \n" ^
  " JUMP("^lbl^"); \n"^
  lbl_jmp^": \n" ^
  "MOV(SP,R2); \n" ^ 
  "JUMPA(INDD(R0,2)); \n"
;;

let construct_constants_table exprs =
  let rec remove_dups = function
    |[]->[]
    |x::xs -> if List.mem x xs then remove_dups xs else x :: remove_dups xs in

  let rec top_sort = function
    |Pair(a,b) -> top_sort a @ top_sort b @ [Pair (a,b)] 
    |Symbol s -> [String s; Symbol s]
    |Vector ls -> 
        List.fold_left 
          (fun a b -> 
              a @ top_sort b)
          [] ls @ [Vector ls]           
    |e -> [e] in
  let rec consts = function
    |Const' e-> [e]
    |BoxSet'(_,e) |Set'(_,e) |Def' (_,e) -> consts e
    |If'(test,dit,dif) -> consts test @ consts dit @ consts dif 
    |Seq' exprs | Or' exprs ->List.fold_left (fun a b -> a @ consts b) [] exprs
    |LambdaOpt'(_,_,e)|LambdaSimple'(_,e) -> consts e
    |Applic'(proc,argl) | ApplicTP'(proc,argl) -> 
       consts proc @ (List.fold_left (fun a b-> a @ consts b) [] argl)
    | _ -> []
  in
  let init = List.map fst Constants.init in
  let all_consts = List.fold_left (fun a b-> 
                               a @ consts b) init exprs in
  let all_consts = remove_dups all_consts in
  let after_sort = List.rev (List.flatten (List.map top_sort all_consts)) in
  Constants.create_table (List.rev (remove_dups after_sort))

;; 
      


let code_gen e =
  let rec sexpr_gen e =
    let addr= 
      string_of_int (Constants.find e (Constants.table())) in
    "MOV(R0,IMM("^addr^")); \n" 
  in
  let delta = 2 in
  let var_gen = function
    |VarFree' v -> 
        let global = Global_Env.get_env() in
        if List.mem_assoc v global then
          let add = string_of_int(List.assoc v global) in 
          "MOV(R0,IND("^add^")); \n"^
          "CMP(R0,T_UNDEFINED); \n"^
          "JUMP_EQ(UNDEFINED_VARIABLE_ERROR);\n" 
        else "printf(\"Exception: variable " ^v ^" is not bound\\n \"); \n"^
        "exit(12);"
    |VarParam' (name,minor) ->
        let minor = minor+delta in 
        "MOV(R0,FPARG("^(string_of_int minor) ^"));\n" 
    |VarBound' (name,major,minor) ->
        let minor = string_of_int minor  in
        let major = string_of_int major in  
        "MOV(R0,FPARG(0));\n"^
        "MOV(R0,INDD(R0,"^major^"));\n"^
        "MOV(R0,INDD(R0,"^minor^"));\n" in
  let rec run depth = function
    |Const' e ->
        sexpr_gen e
    |Var' v -> var_gen v 
    |LambdaSimple' (params,bdy) ->
       let label = gen_label "lambda" in
       let labelexit = gen_label "exit" in
       let env_sz = string_of_int(List.length depth) in
       let lambda_start = start_of_lambda label labelexit env_sz in

      lambda_start^ run (params::depth) bdy ^
       end_function 
       ^
       labelexit ^": //OUT OF LAMBDA \n"
    |LambdaOpt'(params,optparam,bdy)->
       let label = gen_label "lambda" in
       let labelexit = gen_label "exit" in
       let env_sz = string_of_int(List.length depth) in
       let lambda_start=start_of_lambda label labelexit env_sz in
       lambda_start^lambda_op_fix (List.length params)^run (params::depth) bdy^
       end_function^labelexit^": //OUT OF LAMBDA \n"
       

    |Applic'(proc,argl) ->
       let applic_bdy = applic_bdy (run depth) argl proc in
      applic_bdy ^ normal_applic_suffix

    |ApplicTP'(proc,argl) ->
        let applic_bdy = applic_bdy (run depth) argl proc  in
        applic_bdy ^ applic_tp_suffix (string_of_int (List.length argl))

    |If'(test,dit,dif) ->
        let tst_prog,dit_prog,dif_prog =
          run depth test,run depth dit, run depth dif in
        let label_else,label_ifexit = gen_label "else", gen_label "if_exit" in
        let cmp_s,jmp_eq,jmp_ex = "\nCMP(R0,FALSE);\n",
                                  "\nJUMP_EQ("^label_else^");\n",
                                  "\nJUMP("^label_ifexit^");\n" in
       tst_prog ^ cmp_s ^ jmp_eq ^ dit_prog ^ jmp_ex
       ^ label_else ^":\n"^ dif_prog^ label_ifexit^":\n"
    
    |Seq' exprs ->
        List.fold_left 
          (fun a b-> 
            let b = run depth b in
             a ^ "\n" ^b)
          ""
          exprs
    |Def' (Var' (VarFree' a), e) ->
        let val_e = run depth e in
        let global = Global_Env.get_env() in
        let addr = string_of_int (List.assoc a global) in
          asm_comment"IN DEFINE" ^
          val_e ^ "\n MOV(IND("^addr^"), R0);\n"^ "MOV(R0,VOID); \n" 
    | _ ->  "pieieieiei" in 
        
          
       
  run [] e
;;
            


open IO;;
let init const_table global_tbl=
   let prol,epi = file_to_string "prologue.c", file_to_string "epilogue.c" in
   let string_lst,strs_len = Constants.create_string_list() in
   let sz =
     List.length const_table+5*List.length global_tbl+2*strs_len in
   let sz = string_of_int sz in
   let ge =  List.map snd global_tbl in
   let malloc =  
   "PUSH(IMM("^sz^"+1)); \n" ^
   "CALL(MALLOC); \n" ^
   "MOV(R0,IMM("^string_of_int Constants.const_tble ^ ")); \n" ^
   "DROP(1); \n \n" in
   let tables = 
   (List.fold_left
     (fun a b ->
       let b = string_of_int b in
       a ^ "MOV(IND(R0),IMM("^b^"));\n INCR(R0);\n") 
     (prol ^ malloc) const_table) ^ 
       
    (List.fold_left (fun a b->
     a^ "MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); \n") 
    "\n /*starting global env------------*/ \n" 
    ge)
    in 
   
    let constants = Constants.get_const_prims() in 
    let prims = Global_Env.foo() in

    constants^"\n"^tables^"\n"^prims^"\n"^string_lst ^"\n", epi
;;
let compile_scheme_file scm_source_file asm_target_file =
  let str=file_to_string scm_source_file in
  let exprs=Tag_Parser.read_expressions str in
  let exprs=List.map Semantics.run_semantics exprs in
  let const_tbl=construct_constants_table exprs;
                (Constants.addr_table()) in
  let global_tbl= Global_Env.create_global_env exprs in
  let prologue,epilogue=init const_tbl global_tbl in
  let asm_strs=List.map code_gen exprs in
  let asm_str=List.fold_left 
      (fun a b-> a ^ "\n /*new expr */ \n \n" ^ b) 
      prologue 
      asm_strs in
  let asm_str=asm_str ^ epilogue in
  Constants.reset_const_tbl(); Global_Env.reset_env();
  string_to_file asm_str asm_target_file

;;

end;;

let compose f g x = f(g x) ;;
let tst = compose Code_Gen.code_gen Semantics.run_semantics;;
let tst = compose tst Tag_Parser.read_expression;;
let printtst x = 
  let x= tst x in
  Printf.printf "%s" x;;
let rd_expz str =
  let sz = Tag_Parser.read_expressions str in
  List.map Semantics.run_semantics sz;;

let foo ()=  
  Code_Gen.compile_scheme_file "foo.scm" "goo.c";
  Sys.command "gcc -o out goo.c";
  Sys.command "./out";;
