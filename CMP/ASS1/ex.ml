




(* Grammer for A -> 'a' *)

let a_p = const (fun x -> x='a');; 

(*Grammer for B -> 'b' *) 

let b_p = const (fun x-> x='b');;

(*Grammer for A UNION B -> a or b *) 

let a_or_b = disj a_p b_p;;

(*Grammer for A*B -> ab *) 

let ab_p = caten a_p b_p;;

(*Grammer for A*B union A *)

let ab_a = disj ab_p a;; 

(*This gives a miss matching types error, ab_p returns (char*char)*char list 
 * whereas a returns char * char list, we want ab_p to return char * char list
 * *)

let ab_p = pack ab_p (fun (a,b) -> "passed");;

let ab_a = disj ab_p a;; 

(*Still wont work since ab_p string * char list *)

let a = pack a (fun x-> "passed");;

(*Now both a and ab_p have the same type we can use disj *)

let ab_a = disj a ab_p;; 

(*Now this works since both a and ab_p are of the same type
 * This is a grammer that accepts {a,ab} *)
