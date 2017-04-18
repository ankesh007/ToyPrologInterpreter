open Types;;
open MGU;;

let un_var = ref 0 (* used by get_unique_var *)
let found1 = ref false
let want_more = ref true 

let get_unique_var() =
    (un_var := !un_var+1; "___UNIQUE_VAR"^(string_of_int !un_var))

let turnOnfound() =
	(found1 := true; ())



let rec print_term a termSym = match termSym with V(x) -> print_string (x^": "); |
Node(a,b) -> if a= "TermString" then print_string " " else (print_string (a^" " )); let yy=(List.fold_left (print_term) () b) in print_string (" "); yy ;;

let rec print_clause x = match x with SingleClause(x) -> (print_term () x) |
ClauseImplication(x,y) -> (print_term () x); (print_term () y);;

let rec  print_database dataBase = match dataBase with x::xs -> print_clause x ; print_newline(); print_database xs 
| [] -> ();; 


let rec print_answer list1 = match list1 with x::xs -> 
(match x with Binding (a,b) -> print_term () a;print_term () b; print_newline(); (print_answer xs) )
| []->() ;;

let rec replaceTerm termSym cons_val = match termSym with V(x) -> V(x^cons_val) 
| Node(a,b) -> Node(a,(List.map (fun x-> (replaceTerm x cons_val)) b));;

let rec replaceClause single_clause cons_val = match single_clause with SingleClause(a) -> SingleClause (replaceTerm a cons_val) |
ClauseImplication(a,b) -> ClauseImplication(replaceTerm a cons_val,replaceTerm b cons_val);; 

let rec replacement repDatabase cons_val = match repDatabase with x::xs ->
(replaceClause x cons_val)::(replacement xs cons_val) |
[] -> [] ;;

(* compose_first augment_goal_list *)

let rec composeTerm termSym list2 = match list2 with x::xs ->
(match termSym with Binding(a,b) -> (match x with Binding(c,d) -> 
if b=c then Binding(a,d) else (composeTerm termSym xs)))
| [] -> termSym ;;

let rec compose_first list1 list2 = match list1 with 
x::xs -> (composeTerm x list2)::(compose_first xs list2)
| [] -> [];;

let getHead list1 = match list1 with x::xs -> x | [] -> raise Error1;;

let rec augment_goal_list goalList newList = match newList with Node(a,b) ->
(if a="TermAnd" then (match b with x::xs -> x::(augment_goal_list goalList (getHead xs)) | []->raise Error1 ) 
else if a="TermOr" then (match b with x::xs -> x::(augment_goal_list goalList (getHead xs)) | []->raise Error1)
else (newList::goalList)) | V(x) ->raise Error1;;


(* 
print_substitution substitution;
 *)
(* (solve_goallist (List.map (fun x-> (substitute_goal_list x substitution)) xs) (replacement orgDataBase get_unique_var) orgDataBase)) *)

let rec substituteVar var list1 = match list1 with [] -> var |
x::xs -> (match x with Binding(a,b) -> if(var=a) then b else (substituteVar var xs ));;

let rec substitute_goal goal list1 = match goal with V(x) -> (substituteVar (V(x)) list1) |
Node(a,b) -> Node(a,(List.map (fun x-> (substitute_goal x list1)) b));;

let rec goal_solver goal goalList accu curDataBase orgDataBase = match curDataBase with x::xs -> 
(* print_term () goal;
print_newline(); *)
let xx=(goal_clause_solver goal x accu goalList orgDataBase) in (if (!want_more)=false then ([],false) else (goal_solver goal goalList accu xs orgDataBase)) | []-> ([],false);


and goal_clause_solver goal single_clause accu goalList orgDataBase = match single_clause with SingleClause(x)-> 
(let temp_substi= (mgu2 goal x) in (match temp_substi with (a,false) -> ([],false) 
|(a,true) -> 
(solve_goalList (List.map (fun x-> (substitute_goal x a)) goalList) (compose_first accu a) orgDataBase)))
 | ClauseImplication(x,y) -> let temp_substi = (mgu2 goal x) in 
let augment_goalList = (augment_goal_list goalList y) in 
(match temp_substi with (a,false) -> ([],false) | (a,true) ->
(solve_goalList (List.map (fun x-> (substitute_goal x a)) augment_goalList) (compose_first accu a) orgDataBase))

and solve_goalList goalList accu orgDataBase = match goalList with x::xs -> 
(let substitution =(goal_solver x xs accu (replacement orgDataBase (get_unique_var())) orgDataBase)
in ([],false))
(* in (solve_goallist  (List.map (fun x-> (substitute_goal_list x substitution)) xs) (compose_first accu substitution) orgDataBase) ) *)
| [] -> 

print_answer accu;

print_string "Yes";
turnOnfound();
print_newline();

let inp= (read_line()) in
if inp="." then want_more:=false
else want_more:=true;

(* print_answer accu ; *)
([],false);;

let check_empty list1 = match list1 with [] -> true |
x::xs -> false;;

let interpret goal orgDataBase = let temp2 = (get_var_list goal (Node("a",[]))) in
 let temp= (goal_solver goal [] temp2 (replacement orgDataBase (get_unique_var())) orgDataBase ) in
  if (check_empty temp2)=true then
   if (!found1)=false then (print_string "No";print_newline()) else print_string ""
   else (print_string "No More";print_newline());;

