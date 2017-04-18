open Types;;
(* (* #use "mgu2.ml";; *)
exception Invalid_Signature of string;;
exception Invalid_Preterm of string;;
exception ILL_FORMED_PRETERM;;
exception NOT_UNIFIABLE;;
exception NOT_SUBSTITUTABLE;;

module VarSet = Set.Make(String);;

type variable = string;;
type symbol = string;;
type term = V of variable | Node of symbol * (term list);;

type sym_arity = SymArity of symbol*int;;
type sym_arity_list = (sym_arity list);;

type homomorphic_extnsn_sbst = Binding of term * term;;

type sigm = Sigma of variable * term;;
 *)
(* let s=VarSet.empty;;
let clr = Sys.command ;;
 *)

let rec present a acc = match acc with 
[] ->false | x::xs -> if a=x then true else present a xs;;


let rec check_sig_internal signature acc = match signature with 
[] -> true | x::xs -> match x with SymArity(a,b) -> 
if b<0 then raise (Invalid_Signature ("Negative arity of "^a) )
else if (present a acc) then raise (Invalid_Signature ("Reapeated Symbol "^a) )
else (check_sig_internal xs (a::acc)) ;; 

let rec check_sig list_pair = check_sig_internal list_pair [];;
(* 
check_sig [SymArity("f",1);SymArity("g",1);SymArity("h",10);];;
 *)
let rec find_arity x signature = match signature with []->
raise (Invalid_Preterm ("Invalid Symbol"^ x )) | y::xs -> match y with 
SymArity(a,b) -> if x=a then b else find_arity x xs;;

let rec get_list_length list1 = match list1 with []->0 
| x::xs -> 1 +  (get_list_length xs);;

let rec check_list y signature= match y with []->true 
| x::xs-> ((wfterm x signature) && (check_list xs signature))

and wfterm termSym signature = match termSym with V(x)->true
| Node(x,y)-> let arity = find_arity x signature in 
let list_length =  get_list_length y in if arity = list_length then check_list y signature else 
raise ILL_FORMED_PRETERM;;

let rec height_children a termSym = max (height termSym) a
(* Sys.command "clear";; *)
and height  termSym = match termSym with V(x)->1
| Node(x,y)-> 1+ (List.fold_left (height_children) 0 y);;

let sum a b = a+b;;

let rec size termSym = match termSym with V(x) -> 1
| Node(x,y) -> 1 + List.fold_left sum 0 (List.map size y);;

size (Node("X",[V("1");V("@");V("11")]));;


let print_set s = VarSet.iter print_endline s;;


let rec list_var termSym = match termSym with V(x) -> 
VarSet.singleton x | Node (a,b) -> (List.fold_left VarSet.union VarSet.empty (List.map list_var b));;

let temp= (Node("V",[(V("X"));(V("X"));(V("ZZ"))]));;
let temp2 = Node("V",[]);;

print_set (list_var temp2);;
print_set (list_var temp);;

let rec append a b = match a with []-> [b] | x::xs -> x::(append xs b);; 


let rec substitute_Simple a substitution = match substitution with []-> raise (NOT_SUBSTITUTABLE) 
| x::xs -> (match x with Sigma(var,value)-> if a=var then value else (substitute_Simple a xs));;

let rec subst termSym substitution = match termSym with V(x) ->
(substitute_Simple x substitution) | Node(a,b) -> 
Node(a,(List.fold_left append [] (List.map (fun x-> (subst x substitution)) b)));;

let rec substitute a substitution = match substitution with []-> raise (NOT_SUBSTITUTABLE) 
| x::xs -> (match x with Binding(var,value)-> if a=var then value else (substitute a xs));;

let rec homomorphic_subst termSym substitution = match termSym with V(x) ->
(substitute termSym substitution) | Node(a,b) -> 
Node(a,(List.fold_left append [] (List.map (fun x-> (homomorphic_subst x substitution)) b)));;

let rec recursive_check a d = match d with V(s) -> true | Node(x,y)-> let temp = (list_var d) in 
(match a with V(z) -> if(VarSet.mem z temp) then false else true | Node(aa,bb)->true );;

let rec compose subExtnd list1 = match list1 with [] -> subExtnd |
x::xs -> (match subExtnd with Binding(a,b) -> 
	(match x with Binding(c,d)-> if b=c then (Binding(a,d)) else (compose subExtnd xs)));;

let rec merge list1 list2 = match list1 with [] -> list2 |
x::xs1 -> (compose x list2)::(merge xs1 list2);;


let rec type_convert x = match x with []->[] |
a::b -> (Binding(V(a),V(a)))::(type_convert b);;

let get_var_list t1 t2 = let set1 = (list_var t1) in let set2 = (list_var t2) in 
let varU = (VarSet.union set1 set2) in (type_convert (VarSet.elements  varU)) ;; 

let rec duplicate_detector list1 a = match list1 with [] -> [a] 
|  x::xs -> if a=x then list1 else x::(duplicate_detector xs a);;

let unique list1 = List.fold_left duplicate_detector [] list1;;


let rec final_check list1 = match list1 with []->([],true)|
x::xs -> (match x with Binding(a,b) -> if (recursive_check a b) then (let temp = (final_check xs) in 
(match temp with (yy,false) -> ([],false) | (yy,true) -> (x::yy,true))) else ([],false)) ;;

let rec compose_merge list1 list2 acc = match list1 with 
[] ->( match list2 with []-> (acc,true) | x::xs -> ([],false))
 | x1::xs1 -> (match list2 with []-> ([],false) | x::xs ->
 	let temp_substi = (mgu2 (homomorphic_subst x1 acc) (homomorphic_subst x acc)) 
 in match temp_substi with (a,false) -> ([],false) | 
(a,true) -> (compose_merge xs1 xs (unique(merge acc a))) )


and mgu2 t1 t2 = match t1 with V(a)-> if t1=t2 then ([],true) else ([Binding(t1,t2)],true) 
| Node(a,b) -> (match t2 with V(x)-> ([Binding(t2,t1)],true) | Node(x,y) -> if (a=x) then 
	let temp = (compose_merge b y (get_var_list t1 t2)) in (match temp with (un,true)->
(final_check un) | (un,false) -> ([],false)) else  ([],false) );;
