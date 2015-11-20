(*We want to define a parser for the operations + or - *)

#use "pc.ml"
open PC;;

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

let ignore nt = 
  fun s ->
    let (e, s) = (nt s) in
    ([],s);;
let ignore_wh = ignore (star nt_whitespace);;


let foo = function
  |'+'->ScmAdd 
  |'-'->ScmSub
  |'*'->ScmMul
  |'/'->ScmDiv ;;
let op = 
  let plu = char '+' in
  let minu = char '-' in
  let mul = char '*' in
  let div = char '/' in
  let op = disj_list [plu;minu;mul;div] in 
  pack op foo;;
   (*pack op (fun x -> function 
              | '+' -> ScmAdd
              | '-' -> ScmSub);; 
*)

let make_char_value base_char displacement =
  let base_char_value = Char.code base_char in
  fun ch -> (Char.code ch) - base_char_value + displacement;;

let nt_digit_0_9 = pack (range '0' '9') (make_char_value '0' 0);;
  
let nt_digit_1_9 = pack (range '0' '9') (make_char_value '0' 0);;


let nt_nat = 
  let nt = range '1' '9' in
  let nt = pack nt (make_char_value '0' 0) in
  let nt'= range '0' '9' in
  let nt'= pack nt' (make_char_value '0' 0) in 
  let nt'= star nt' in
  let nt = caten nt nt' in
  let nt = pack nt (fun (d,ds) -> d::ds) in 
  let nt = pack nt (fun s -> List.fold_left (fun a b -> a*10 +b) 0 s) in
  let nt'= char '0' in
  let nt''= char '0' in
  let nt''_= range '0' '9' in
  let nt'' = caten nt'' nt''_ in 
  let nt' = diff nt' nt'' in 
  let nt' = pack nt' (fun e -> 0) in
  let nt = disj nt nt' in
  nt;;

let my_nat = pack nt_nat (fun x -> ScmInt x);;


let mini_exp =
  let rb = char '(' in 
  let nt_star = plus my_nat in
  let mini_exp= caten op nt_star in
  let mini_exp = pack mini_exp (fun (a,b) -> ScmOp(a,b)) in
  let lb = char ')' in 
  let mini_exp = caten rb mini_exp in
  let mini_exp = caten mini_exp lb in 
  let mini_exp = pack mini_exp (fun ((a,b),c) -> b) in
  disj my_nat mini_exp
;; 


let rec mxp2 = 
  let rb  = char '(' in
  let lb = char ')' in
  let op = caten op ignore_wh in
  let op = pack op (fun (a,b) -> a) in
  let nums = caten my_nat ignore_wh in
  let nums = pack nums (fun (a,b) -> a) in
  let nums = star nums in 
  let mxp = caten op nums in
  let mxp = pack mxp (fun (a,b) -> ScmOp(a,b) )in
  let mxp = caten rb mxp  in
  let mxp = caten mxp lb in
  let mxp = pack mxp (fun ((a,b),c) -> b) in
  (*let mxp = disj my_nat (delayed (fun ()-> mxp2)) in *)
  mxp;;


let make_ign_whs nt = 
  let nt = caten nt ignore_wh in
  let nt = pack nt (fun (a,b)->a) in
  let nt = caten ignore_wh nt in
  let nt = pack nt (fun (a,b) -> b) in 
  nt;;


let exps2 =
   let rec make () =
    let rec mxp2 () = 
    let rb  = make_ign_whs (char '(') in
    let lb = make_ign_whs (char ')')  in
    let op = make_ign_whs op in
    let nums = plus (delayed make) in
    let nums = make_ign_whs nums in
    let mxp = caten op nums in
    let mxp = pack mxp (fun (a,b) -> ScmOp(a,b) )in
    let mxp = caten rb mxp  in
    let mxp = caten mxp lb in
    let mxp = pack mxp (fun ((a,b),c) -> b) in
    mxp in
    let my_nat = caten my_nat ignore_wh in
    let my_nat = pack my_nat (fun (a,b) ->a) in
   disj my_nat (mxp2()) in
   make()
 ;;


