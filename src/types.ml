
(* --- Prolog syntax elements constructors --- *)
exception Invalid_Signature of string;;
exception Invalid_Preterm of string;;
exception ILL_FORMED_PRETERM;;
exception NOT_UNIFIABLE;;
exception NOT_SUBSTITUTABLE;;
exception Error1;;

type symbol = string ;;
type variable = string ;;
type term =  V of variable | Node of symbol * (term list);;

module VarSet = Set.Make(String);;
type homomorphic_extnsn_sbst = Binding of term * term;;
type sym_arity = SymArity of symbol*int;;

type sym_arity_list = (sym_arity list);;


type sigm = Sigma of variable * term;;

let s=VarSet.empty;;

type clause =                                   (* database clauses *)
  | ClauseImplication of term * term            (* implication clause *)
  | SingleClause of term                        (* fact clause *)

    (* prolog term types *)

(*   | TermOr of term * term                       (* logical disjunction *)
  | TermAnd of term * term                      (* logical conjuction *)
  | TermString of string                        (* prolog string term *)
  | TermConstant of constant                    (* prolog constant term *)
  | TermVariable of name                        (* prolog variable terms *)
  | TermFunctor of name * arguments             (* functor term *)
  | TermNegation of term                        (* not *)
  | TermTermUnify of term * term                term1 = term2
  | TermIs of term * term                       (* term1 is term2 *)
  | TermArithmeticLess of term * term           (* term1 < term2 *)
  | TermArithmeticGreater of term * term        (* term1 > term2 *)

  | TermArithmeticPlus of term * term           (* term1 + term2 *)
  | TermArithmeticMinus of term * term          (* term1 - term2 *)
  | TermArithmeticDiv of term * term            (* term1 / term2 *)
  | TermArithmeticMult of term * term           (* term1 * term2 *)
  | TermList of lists                           (* prolog lists *)
  | TermCut                                     (* ! *)
 *)
 


  (* | TermTermNotUnify of term * term             (* term1 \= term2 *) *)
  (* | TermDecomposition of term * term            (* term1 =.. term2 *)
  | TermTermEquality of term * term             (* term1 == term2 *)
  | TermTermInequality of term * term           (* term \== term2 *)
  | TermArithmeticEquality of term * term       (* term1 =:= term2 *)
  | TermArithmeticInequality of term * term     (* term1 =\= term2 *) *)
  (* | TermArithmeticGeq of term * term            (* term1 >= term2 *)
  | TermArithmeticLeq of term * term            (* term1 <= term2 *)
  | TermTermOrderEquality of term * term        (* term1 @= term2 *)
  | TermTermOrderInequality of term * term      (* term1 @\= term2 *)
  | TermTermOrderLess of term * term            (* term1 @< term2 *)
  | TermTermOrderGreater of term * term         (* term1 @> term2 *)
  | TermTermOrderGeq of term * term             (* term1 @>= term2 *)
  | TermTermOrderLeq of term * term             (* term1 @=< term2 *)

  | TermModule of term * term                   (* term1 : term2 *) *)

(*   | TermArithmeticRemainder of term * term      (* term1 rem term2 *)
  | TermArithmeticModulo of term * term         (* term1 mod term2 *) *)
 (*  | TermArithmeticDivs of term * term           (* term1 divs term2 *)
  | TermArithmeticMods of term * term           (* term1 mods term2 *)
  | TermArithmeticDivu of term * term           (* term1 divu term2 *)
  | TermArithmeticModu of term * term           (* term1 modu term2 *) *)
  (* | TermArithmeticIntDiv of term * term         (* term1 // term2 *) *)
  (* | TermArithmeticRightShift of term * term     (* term1 >> term2 *) *)
  (* | TermArithmeticLeftShift of term * term      (* term1 << term2 *) *)
  (* | TermArithmeticPower of term * term          (* term1 ** term2 *) *)
  (* | TermVariableInstantiated of term            (* ^term1 *) *)
  (* | TermBitwiseAnd of term * term               (* /\ *) *)
  (* | TermBitwiseOr of term * term                (* \/ *) *)
  (* | TermBitwiseNot of term * term               (* \ *) *)


(* and lists =
  | EmptyList
  | NormalList of arguments
  | DividedList of arguments * term

and arguments = term list                (* functor arguments *)

and constant =  
  | ConstantAtom of name                 constants: atom
  | ConstantNumber of number             (* constant: number *)

and number =
  (* | Float of float      (* prolog float *) *)
  | Integer of int      (* prolog integers *)

and name = string       (* prolog names *)
;; *)

