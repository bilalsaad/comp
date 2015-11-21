#use "compiler.ml";;


let test s n = Printf.printf "Starting test %s %n\n" s n;;
exception X_test_fail of int;;
let test_failed n = X_test_fail n;;

let assert_eq a b n = 
  if a = b then Printf.printf "Test %n passed \n" n
  else raise (test_failed n)
    


(* Testing parser *)
let parse_test to_test to_eq n =
   let res = Parser.read_sexpr to_test in
   assert_eq res to_eq n;;

let num_test = test "Test on numbers " ;;


num_test 0;;

parse_test "1" (Number (Int 1)) 0;;


num_test 1;;
parse_test "0x3/2" (Number(Fraction {numerator = 3; denominator = 2})) 1;; 




