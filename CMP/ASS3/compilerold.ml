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
  



let rec sexpr_to_string = function
  | Void -> "void"
  | Bool e -> if e then "#t" else "#f" 
  | Nil -> "()"
  | Number e -> (match e with
            | Int x -> (string_of_int x)
            | Fraction {numerator=a;denominator=b} ->
                (string_of_int a) ^ "/" ^ (string_of_int b))

  | Char e -> "#\\" ^ (list_to_string [e]) 
  | String e -> e
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
      let bdy = List.fold_right (^) bdy "" in
      "#( " ^ bdy ^ ")";;
            
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
  let nt_nline = char '\n' in
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
 

let nt_nat =
  let nt = range '1' '9' in
  let nt = pack nt (make_char_value '0' 0) in
  let nt' = range '0' '9' in
  let nt' = pack nt' (make_char_value '0' 0) in
  let nt' = star nt' in
  let nt = caten nt nt' in
  let nt = pack nt (fun (d, ds) -> (d :: ds)) in
  let nt = pack nt (fun s -> List.fold_left (fun a b -> a * 10 + b) 0 s) in
  let nt' = char '0' in
  let nt'' = char '0' in
  let nt''' = range '0' '9' in
  let nt'' = caten nt'' nt''' in
  let nt' = diff nt' nt'' in
  let nt' = pack nt' (fun e -> 0) in
  let nt = disj nt nt' in
  nt;;


(*END OF MAYER INT and NAT*)

let nt_scm_int =
  pack nt_int (fun x-> Int x);;
let nt_scm_uint =
  pack nt_nat (fun x -> Int x);;
(*now we want create a parser for the fractions *)
(* fractions have num/denom, where
 * num -> int
 * denom ->uint*)
(* may need to ignore 0 here... *)
(*may need to add hexa? fuck*)
let nt_fract = 
  let nt_numer = nt_int in
  let nt_slash = char '/' in 
  let nt_denom = nt_nat in
  pack (caten nt_numer (caten nt_slash nt_denom)) 
       (fun (a,(_,b)) -> 
          let numerator = a in
          let denominator = b in
         Fraction {numerator ; denominator});;
(*now we combine to create nt_number *)
let nt_number =
  pack (disj nt_fract nt_scm_int) (fun x -> Number x) ;;


(*Now we need to write a parser for le scheme symbols - 3.2.3*)
let nt_symbol =
  let nt_lowercase = pack (range 'a' 'z') Char.uppercase in
  let nt_uppercase = range 'A' 'Z' in
  let nt_digits = range '0' '9' in
  let nt_punctuation=disj_list[(char '!');(char '$');(char '^');
                              (char '^');(char '*');(char '-');
                              (char '_');(char '=');(char '+');
                              (char '<');(char '>');(char '/');
                              (char '?')] in
  let nt_part_of_symbol = disj_list[nt_lowercase;nt_uppercase;nt_digits;
                                     nt_punctuation] in
  let nt_body_of_symbol = plus nt_part_of_symbol in
 (* let nt_symbol = caten (char ''') nt_body_of_symbol in*)
  pack nt_body_of_symbol (fun b -> Symbol (list_to_string b));;

(*Now we deal with a parser for String - 3.2.4*)
(*NEED TO DEAL WITH META CHARACTERS*)
let nt_string =
  let nt_quote = char '"' in
  let meta_characters=[(1,(word "\\n"));(2,(word "\\r"));
                        (3,(word "\\t"));(4,(word "\\\\"));
                        (5,(word "\\\"")) ] in
  let meta_characters = List.map (function
    | (1,e) -> pack e (fun _ -> '\n')
    | (2,e) -> pack e (fun _ -> '\r')
    | (3,e) -> pack e (fun _ -> '\t')
    | (4,e) -> pack e (fun _ -> '\\')
    | (5,e) -> pack e (fun _ -> '\"')
    | _ -> raise (err "meta characters error")) meta_characters in
  let nt_meta = disj_list meta_characters in
  let nt_m_any = diff nt_any nt_meta in
  let nt_m_any = disj nt_m_any nt_meta in
  let nt_string =
    caten nt_quote(caten (star (diff nt_m_any nt_quote)) nt_quote)
     in
  pack nt_string (fun (_,(s,_)) ->String (list_to_string s));;
              
(*String not good*)

(*Now we do the Char parser *)
let nt_char = 
  let nt_prefix = word "#\\" in
  let nt_named_chars = disj_list
                         [(word_ci "newline");(word_ci "return");
                         (word_ci "tab");(word_ci "page"); 
                         (word_ci "lambda")] in
  let nt_named_chars = pack nt_named_chars 
    (fun s -> match (list_to_string s) with
      "newline" -> Char '\n'
    | "return" ->  Char '\r'
    | "tab" -> Char (Char.chr 12)  
    | "lambda"  -> Char (Char.chr 12)  
    | _ -> Char 'x'
    ) 
  
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
    let rec exp () =
      let expr = plus (delayed make) in
      pack (caten nt_lb (caten expr nt_rb)) (fun (_,(ls,_)) -> ls) 

    and nt_pair = 
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
     let lst =  [nt_void;nt_bool;nt_nil;
                nt_number;nt_string;
                nt_pair;nt_symbol;nt_vector;nt_char;nt_quote] in
     disj_list (List.map (add_skip test) lst)  
  in
  make();;


let read_sexpr str =
  let str = string_to_list str in
  match (nt_sexpr str) with
  |(a,_) -> a 
  | _ -> raise (err "did not fully parse") 

let read_sexprs string = raise X_not_yet_implemented;;

end;;

 
type expr =
  | Const of sexpr
  | Var of string
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list)
  | ApplicTP of expr * (expr list);;

exception X_syntax_error;;

module type TAG_PARSER = sig
  val read_expression : string -> expr
  val read_expressions : string -> expr list
  val expression_to_string : expr -> string
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

(*checks if a given pair , is a proper list*)
let rec is_proper = function
  | Pair(_,Nil) -> true
  | Pair(_,Pair(x,y)) -> is_proper (Pair (x,y))
  | _ -> false;;

(*converts a Pair proper list into a list of sexpressions*)
let rec pair_to_list = function
  | Pair(e1,Nil) -> [e1] 
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

let rec tag_parse = function
  | (Char _) as c -> Const c | (Number _) as n -> Const n 
  | (Bool _) as b ->Const b | (String _) as s -> Const s | Void -> Const Void 
  | Pair (Symbol "quote", Pair (e,Nil)) -> Const e 
  | Pair (Symbol "unquote", Pair (e,Nil)) -> Const e 
  |(Pair((Symbol("DEFINE")), (Pair((Symbol(var)),
   (Pair( exp, Nil)))))) -> Def ( Var var, tag_parse exp)
  (*need to check var against list of reserved words*)
  |(Pair((Symbol("IF")), (Pair(test, (Pair(dit, (Pair(dif, Nil)))))))) ->
  If (tag_parse test, tag_parse dit, tag_parse dif)
  |(Pair((Symbol("IF")), (Pair(test, (Pair(dit, Nil)))))) ->
      If(tag_parse test,tag_parse dit, tag_parse Void)
  |Pair (Symbol "LAMBDA", Pair (ls, Pair(bdy,Nil))) -> 
      let get_sym = (function |Symbol str -> str 
                             |_->raise (err "Non symbol in lambda")) in
      if is_proper ls then 
        let args = pair_to_list ls in
        let args = List.map get_sym args in
        LambdaSimple (args, tag_parse bdy)
      else
        let (args,variadic) = improper_to_list [] ls in
        let args = List.map get_sym args in
        let variadic = get_sym variadic in
        LambdaOpt (args, variadic, tag_parse bdy)

  (*Or expression*)
  |Pair (Symbol "OR", ls) ->
      let exprls = List.map tag_parse (pair_to_list ls) in
      Or exprls

  (*Application shit*)
  | Pair (func, args) ->
      Applic (tag_parse func,
            List.map tag_parse (pair_to_list args))
 (* |  _ -> Const (Symbol "not yet") *)
;;
let read_expression string = tag_parse (Parser.read_sexpr string);;

let read_expressions string = List.map tag_parse (Parser.read_sexprs string);;

let expression_to_string expr = raise X_not_yet_implemented;;
  
end;; (* struct Tag_Parser *)

let test_parser string =
  let expr = Tag_Parser.read_expression string in
  let string' = (Tag_Parser.expression_to_string expr) in
  Printf.printf "%s\n" string';;

