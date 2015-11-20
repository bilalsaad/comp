(* hw1.ml *)

#use "pc.ml";;

(* *)

type reg = Reg of int;;

type imm = Imm of int;;

type opcode = Add | Sub | Mul | Div | Mov;;

type inst =
  | InstRR of opcode * reg * reg
  | InstRI of opcode * reg * imm;;

type scm_op = ScmAdd | ScmSub | ScmMul | ScmDiv;;
  
type expr =
  | ScmInt of int
  | ScmOp of scm_op * expr list;;

type expr' =
  | Uninitialized
  | ScmInt' of int
  | ScmOp' of scm_op * expr' list;;

exception X_This_should_not_happen;;
exception X_Not_yet_implemented;;
exception X_Expression_includes_uninitialized_values;;
exception X_Cannot_parse_expr_string;;
exception X_Cannot_parse_assembly_program_string;;

module type PARSERS = sig
  val nt_assembly_program : char list -> inst list * char list
  val nt_expr : char list -> expr * char list
end;; (* end of signature PARSERS *)

module Parsers : PARSERS = struct 
open PC;;
 


let make_char_value base_char displacement =
  let base_char_value = Char.code base_char in
  fun ch -> (Char.code ch) - base_char_value + displacement;;

let nt_digit_0_9 = pack (range '0' '9') (make_char_value '0' 0);;
  
let nt_digit_1_9 = pack (range '0' '9') (make_char_value '0' 0);;
  
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

let nt_register =
  let nt = char_ci 'r' in
  let nt = caten nt nt_nat in
  let nt = pack nt (fun (_r, n) -> (Reg n)) in
  nt;;

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

let nt_imm =
  let nt =  pack nt_int (fun n -> (Imm n)) in
  let nt = caten (char '$') nt in
  pack nt (fun (a,b) -> b) ;;

let nt_opcode =
  let nt = word_ci "add" in
  let nt = pack nt (fun _ -> Add) in
  let nt' = word_ci "sub" in
  let nt' = pack nt' (fun _ -> Sub) in
  let nt'' = word_ci "mul" in
  let nt'' = pack nt'' (fun _ -> Mul) in
  let nt''' = word_ci "div" in
  let nt''' = pack nt''' (fun _ -> Div) in
  let nt'''' = word_ci "mov" in
  let nt'''' = pack nt'''' (fun _ -> Mov) in
  let nt = disj nt (disj nt' (disj nt'' (disj nt''' nt''''))) in
  nt;;

(* add your own code here after this comment *)
let ignore_wh =  star nt_whitespace;;

(* when packing the op parser, we want to create the apporpriate types *)
let op_type = function
  |'+'->ScmAdd 
  |'-'->ScmSub
  |'*'->ScmMul
  |'/'->ScmDiv
  | _ -> raise X_Cannot_parse_expr_string 
;;
let add_skip nt = 
  let nt = caten nt ignore_wh in
  let nt = pack nt (fun (a,b)->a) in
  let nt = caten ignore_wh nt in
  let nt = pack nt (fun (a,b) -> b) in 
  nt;;

let my_int =
  let nt =  pack nt_int (fun x -> ScmInt x) in
  add_skip nt;;
let nt_imm =
  let nt =  pack nt_int (fun n -> (Imm n)) in
  let dollar_sign = add_skip (char '$') in
  let nt = caten dollar_sign nt in
  pack nt (fun (a,b) -> b) ;;
let op = 
  let plu = char '+' in
  let minu = char '-' in
  let mul = char '*' in
  let div = char '/' in
  let op = disj_list [plu;minu;mul;div] in 
  let op = pack op op_type in
  op (*
  let op = caten op (plus nt_whitespace) in
  pack op (fun (a,_) -> a);;*)






(* the main parser boom boom*)
let nt_expr =
   let rec make () =
    let rec mxp2 () = 
    let rb  = add_skip (char '(') in
    let lb  = add_skip (char ')')  in
    let expr= plus (delayed make) in
    let expr= add_skip expr in
    let mxp = caten op (diff expr nt_int) in
    let mxp = pack mxp (fun (a,b) -> ScmOp(a,b))in
    let mxp = caten rb mxp  in
    let mxp = caten mxp lb in
    let mxp = pack mxp (fun ((a,b),c) -> b) in
    mxp in
     
   disj my_int (mxp2()) in
make()
;;

let nt_start_inst nt =
  let nt_opcode = add_skip nt_opcode in 
  let nt_register = add_skip nt_register in
  let nt_register1 = caten nt_register (char ',') in
  let nt_register1 = add_skip (pack nt_register1 (fun (a,b) -> a)) in
  let nt = caten nt_opcode (caten nt_register1 nt) in
  nt
;;

let nt_rr =
  let nt = nt_start_inst nt_register in
  let nt = pack nt (fun (a,(b,c)) -> InstRR (a,b,c)) in
  add_skip nt
;; 

let nt_ri =
  let nt = nt_start_inst nt_imm in
  let nt = pack nt (fun (a,(b,c)) -> InstRI (a,b,c))in
  add_skip nt
;;

let nt_assembly_program = 
  let insts = disj nt_rr nt_ri in
  star insts 
;;





end;; (* end of struct Parsers *)

module type FULL_CYCLE = sig
  val compile_arith : expr -> inst list
  val assembly_program_to_string : inst list -> string
  val decompile_assembly_program : inst list -> expr'
  val expr'_to_string : expr' -> string
  val full_cycle : string -> string
end;; (* end of signature FULL_CYCLE *)

module Full_Cycle : FULL_CYCLE = struct

let apply_append s = List.fold_right (@) s [];;

let find_max_register insts =  
  1 + (List.fold_right max 
		       (List.map
			  (function
			    | InstRI(_, (Reg i), _) -> i
			    | InstRR(_, (Reg i), (Reg j)) -> (max i j))
			  insts)
		       0);;

(* add your own code after this *)
let get_op = function
  | ScmAdd -> Add 
  | ScmSub -> Sub
  | ScmDiv -> Div
  | ScmMul -> Mul;;

let rec comp_simple2 reg aop = function
  | ScmInt x -> [InstRI (aop, reg, Imm x)] 
  | ScmOp (op,xs) -> match xs with
                    | [] -> raise X_This_should_not_happen
                    | x :: [] -> comp_simple2 reg aop x 
                    | x :: xs ->
                       let s =List.hd (comp_simple2 reg Mov x) in
                       let op = get_op op in
                       let s = s :: List.fold_right
                               (fun a b ->
                                 (List.hd (comp_simple2 reg op a)) :: b) 
                               xs
                               []
                       in s
;;

let is_simple_expr = function 
  | ScmInt _ -> true 
  | ScmOp(_, xs) -> 
      andmap (fun expr -> match expr with
                | ScmInt x -> true
                | _ -> false) xs;;
let is_Scm_int = function
  ScmInt _ -> true
  |ScmOp _ -> false;;


let single_elem_edge_case = function
  |ScmOp(ScmAdd, [expr]) -> ScmOp(ScmAdd, [expr; ScmInt 0])
  |ScmOp(ScmMul, [expr]) -> ScmOp(ScmMul, [expr; ScmInt 1])
  |ScmOp(ScmDiv, [expr]) -> ScmOp(ScmDiv, [ScmInt 1; expr])
  |ScmOp(ScmSub, [expr]) -> ScmOp(ScmSub, [ScmInt 0; expr])
  | _ -> raise X_This_should_not_happen 
;;

      
(* If I have time I should make this function prettier *) 
let rec compile_arith_helper reg = function
  | ScmInt x as exp-> comp_simple2 reg Mov exp
  | ScmOp (s_op, [x]) as exp  -> 
      let exp = single_elem_edge_case exp in
      compile_arith_helper reg exp
  | ScmOp (s_op, xs) as exp -> match is_simple_expr exp with
                | true -> comp_simple2 reg (get_op s_op) exp 
                | false -> 
                    let op = get_op s_op in
                    let n_reg = (function |Reg x -> x) reg in
                    let n_reg = Reg (n_reg + 1) in               
                    let ans = List.fold_right
                              (fun a b ->  
                                  let n_reg = 
                                    if List.hd xs = a
                                         then reg
                                     else n_reg in
                                  if is_Scm_int a &&  List.hd xs != a 
                                  then
                                    let tmpval = ScmOp (s_op,[a]) in
                                    let ans = comp_simple2 reg op tmpval in
                                    ans @ b
                                  else
                                      let ans=compile_arith_helper n_reg a in
                                      let ans =
                                      if n_reg = reg 
                                          then ans@b 
                                      else 
                                          let inst =InstRR(op,reg,n_reg) in
                                          let ans = ans @ [inst] in
                                               ans @ b
                                     in ans 
                            
                                )
                              xs
                              [] in
                      ans

;;

let compile_arith e = 
    compile_arith_helper (Reg 0) e 
;;
(* Converting CISC AST into test *)




let string_of_op = function
        | Add -> "add " 
        | Mov -> "mov "
        | Sub -> "sub "
        | Mul -> "mul "
        | Div -> "div " ;;
let op_and_reg op a =
  let op = string_of_op op in
  let a = "r" ^ (string_of_int a)  ^", " in
  op^a;;

let ri_line op a b =
  let opa = op_and_reg op a in
  let b = "$" ^ (string_of_int b) ^ "\n" in
  opa^b
;;

let rr_line op a b =
  let opa = op_and_reg op a in 
  let b = "r" ^ (string_of_int b) ^ "\n" in
  opa^b;;
  
let rec assembly_program_to_string = function
  | [] -> "" 
  | x :: xs -> 
      let x_str =   match x with
               | InstRR (op, Reg a, Reg b) -> rr_line op a b 
               | InstRI (op, Reg a, Imm b) -> ri_line op a b 
      in x_str ^ assembly_program_to_string xs;;


exception X_decompile_error of string;;

let err str = X_decompile_error str;;

let debug s  y =
  let y = assembly_program_to_string [y] in 
  Printf.printf "%s\n"  y;;

let rec rem_dups = function
   [] -> [] 
 | _ :: [] as ls -> ls  
 | x :: xs -> if List.mem x xs then
                rem_dups xs 
             else x :: rem_dups xs
;;
let check_valid ls =
  let rg_ls = List.map 
                        (fun x -> match x with
                          | InstRI(_,rg,_) -> rg 
                          | InstRR(_,rg,_) -> rg) ls in
  let rg_ls = rem_dups rg_ls in

  if not (List.mem (Reg 0) rg_ls) then
    false 
  else
    let hash = Hashtbl.create (List.length rg_ls) in
    let is_init = function
      | InstRI(Mov,x,_) -> Hashtbl.add hash x 1; true
      | InstRI(_,x,_)   -> Hashtbl.mem hash x
      | InstRR(Mov,r1,r2) -> 
          Hashtbl.add hash r1  1; Hashtbl.mem hash r2
      | InstRR(_,r1,r2)  -> 
           Hashtbl.mem hash r1 && Hashtbl.mem hash r2 
    in
    andmap is_init ls    
;;



let get_scm_op = function
  |Add->ScmAdd|Mul->ScmMul|Div->ScmDiv|Sub->ScmSub|
  _->raise (err "Used get_scm_op on Mov") 

;;


let rec decomp acc delayed = function
  |[] -> acc 
  |InstRI(op,_,Imm q) :: [] -> 
      let op = get_scm_op op in
      ScmOp'(op, [acc; ScmInt' q])
  |InstRR(op,_,_) :: [] ->
      let op = get_scm_op op in
      ScmOp'(op,delayed @[acc])
  |InstRI(Mov,_,Imm q)::InstRI(Mov,_,Imm n) :: xs -> 
      let delayed = acc :: delayed in
      let delayed = (ScmInt' q) :: delayed in 
      let acc = ScmInt' n in
      decomp acc delayed xs
  |InstRI(Mov,_,Imm q)  :: InstRI(op,_,Imm n) :: xs ->
      let op = get_scm_op op in
      let delayed = acc :: delayed in
      let acc = ScmOp'(op,[ScmInt' q; ScmInt' n])  in
      decomp acc delayed xs
  |InstRI(op,_,Imm q) :: xs -> 
      let op = get_scm_op op in
      let acc = ScmOp'(op, [acc] @ [(ScmInt' q)]) in
      decomp acc delayed xs
  |InstRR(op,_,_) :: xs -> 
      let op = get_scm_op op in
      let pop = List.hd delayed in
      let delayed = List.tl delayed in
      let acc = ScmOp'(op, [pop;acc]) in
      decomp acc delayed xs
;;

                              

let decompile_assembly_program insts = match check_valid insts with
  | false -> Uninitialized
  |  _ ->
    let hd = List.hd insts in
      (match hd with
         InstRI(_,_,Imm x) -> 
             let acc = ScmInt' x in
             decomp acc [] (List.tl insts) 
       |InstRR(_,_,_)  ->  Uninitialized
      ) 
       


(* Expr' to string *)

let rec expr'_to_string = 
  let string_of_scmop = (function
    |ScmAdd->"+"|ScmDiv->"/"|ScmMul->"*"|ScmSub->"-") in    
 function
  |Uninitialized -> raise X_Expression_includes_uninitialized_values
  |ScmInt' x->  " " ^ (string_of_int x)  
  |ScmOp' (op, expr) ->
      let op = string_of_scmop op in
      let bdy =List.map expr'_to_string expr in
      let bdy = List.fold_right 
      (fun a b -> a^ b)
        bdy "" in 
      "(" ^ op ^ " " ^ bdy ^ ")" ;;






(* do not add your own code after this *)

let full_cycle string =
  try (match (Parsers.nt_expr (string_to_list string)) with
       | (expr, []) ->
	  (try (match (Parsers.nt_assembly_program
			 (string_to_list
			    (assembly_program_to_string
			       (compile_arith expr)))) with 
		| (insts, []) ->
		   (expr'_to_string (decompile_assembly_program insts))
		| _ -> raise X_Cannot_parse_assembly_program_string)
	   with PC.X_no_match -> raise X_Cannot_parse_assembly_program_string)
	      | _ -> raise X_Cannot_parse_expr_string)
  with PC.X_no_match -> raise X_Cannot_parse_expr_string;;

end;; (* end of struct Full_Cycle *)
  
(* end of input *)
