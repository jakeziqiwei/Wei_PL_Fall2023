structure TypeCheck : sig

  val typeof : Sugary.term -> Type.typ
end = struct

  structure S = Sugary
  structure T = Type

  fun typeof (S.Nat n) = T.Nat
    | typeof (S.True) = T.Bool
    | typeof (S.False) = T.Bool 
    | typeof (S.Unit) = T.Unit 
    | typeof (S.Add (t1, t2)) = (case ((typeof t1), (typeof t2)) of 
          (T.Nat,T.Nat) => T.Nat
        | (_,_) => raise Fail "error types")
    | typeof (S.Subtract (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Nat,T.Nat) => T.Nat
        | (_,_) => raise Fail "error types")
    | typeof (S.Less (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Nat,T.Nat) => T.Bool
        | (_,_) => raise Fail "error types")
    | typeof (S.Greater (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Nat,T.Nat) => T.Bool
        | (_,_) => raise Fail "error types")          
    | typeof (S.LessEq (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Nat,T.Nat) => T.Bool
        | (_,_) => raise Fail "error types")          
    | typeof (S.GreaterEq (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Nat,T.Nat) => T.Bool
        | (_,_) => raise Fail "error types")
    | typeof (S.Not t1) = (case (typeof t1) of 
          (T.Bool) => T.Bool
        | _ => raise Fail "type Error")
    | typeof (S.And (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Bool,T.Bool) => T.Bool
        | (_,_) => raise Fail "error types")
    | typeof (S.Or (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Bool,T.Bool) => T.Bool
        | (_,_) => raise Fail "error types") 
    | typeof (S.Xor (t1, t2)) = (case (typeof t1, typeof t2) of 
          (T.Bool,T.Bool) => T.Bool
        | (_,_) => raise Fail "error types")  
    | typeof (S.Cond (t1, t2, t3)) = (case (typeof t1, typeof t2, typeof t3) of 
          (T.Bool,typ_1, typ_2) => (if (typ_1 = typ_2) then 
                typ_2
              else 
                raise Fail "error types")
        | (_,_,_) => raise Fail "error types")
    | typeof (S.Eq (t1, t2)) = (case (typeof t1, typeof t2) of 
          (typ_1, typ_2) => (if (typ_1 = typ_2) then
                T.Bool
              else 
                raise Fail "error types"))
    | typeof (S.Pair (t1, t2)) = (case (typeof t1, typeof t2) of 
          (typ_1, typ_2) => T.Product(typ_1,typ_2))
    | typeof (S.First prod) = (case typeof prod of 
          (T.Product (typ_1, _)) => typ_1
        | _ => raise Fail "error types")
    | typeof (S.Second prod) = (case typeof prod of 
          (T.Product( _ , typ_2)) => typ_2
        | _ => raise Fail "error types") 
        
end
