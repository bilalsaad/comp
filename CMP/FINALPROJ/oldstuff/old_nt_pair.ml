
    let rec nt_pair = 
     let nt_lb,nt_rb = add_skip (delayed make)  (char '('),
                        add_skip (delayed make) (char ')') in 
     let nt_sexpr = delayed make in
     let nt_dot = add_skip nt_sexpr (char '.') in
     let nt_proper_list = caten nt_lb (caten (star nt_sexpr) nt_rb) in
     let rec proper_to_pair = (function
       | [] -> Nil
       | x::xs ->  Pair(x,proper_to_pair xs)) in
     
     let rec improper_to_pair = (function
       | [] | [_] -> raise X_this_should_not_happen
       | x :: y :: [] -> Pair(x,y) 
       | x::xs -> Pair(x, improper_to_pair xs)) in
     let nt_proper_list = pack nt_proper_list (fun (_,(ls,_)) -> ls) in
     let nt_proper_list = pack nt_proper_list proper_to_pair in
     let nt_improper_list = caten nt_lb (plus nt_sexpr) in
     let nt_improper_list = caten nt_improper_list nt_dot in
     let nt_improper_list = caten nt_improper_list (caten nt_sexpr nt_rb) in
     let nt_improper_list = pack nt_improper_list
                   (fun (((_,ls),_),(e,_)) -> ls @ [e]) in
     let nt_improper_list = pack nt_improper_list improper_to_pair in
     disj nt_proper_list nt_improper_list 
