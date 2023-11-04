structure Sugary = struct

  datatype term
    = Nat of int (* yes *)
  | True (* yes *)
  | False (* yes *)
  | Unit (* yes *)
  | Add of term * term (* yes *)
  | Subtract of term * term (* yes *)
  | Mul of term * term (* yes *)
  | Pow of term * term (* yes *)
  | Less of term * term (* yes *)
  | Greater of term * term (* yes *)
  | LessEq of term * term(* yes *)
  | GreaterEq of term * term(* yes *)
  | Not of term (* yes *)
  | And of term * term (* yes *)
  | Or of term * term (* yes *)
  | Xor of term * term(* yes *)
  | Cond of term * term * term (* yes *)
  | Eq of term * term (* yes *)
  | Pair of term * term (* yes *)
  | First of term (* yes *)
  | Second of term (* yes *)
  | Var of string (* yes *)
  | Let of string * term * term(* yes *)

end
