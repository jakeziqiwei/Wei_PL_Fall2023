structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

  structure S = Sugary
  structure T = Type

  fun typeof (env, S.Nat n) = T.Nat
    | typeof (env, S.True) = T.Bool
    | typeof (env, S.False) = T.Bool
    | typeof (env, S.Unit) = T.Unit 
    | typeof (env, S.Add(n1,n2)) = (case ((typeof (env,n1)), (typeof (env,n2))) of 
          (T.Nat, T.Nat) => T.Nat
        | (_,_) => raise Fail "not right types add")
    | typeof (env, S.Subtract(n1,n2)) = (case ((typeof (env,n1)), (typeof (env,n2))) of 
          (T.Nat, T.Nat) => T.Nat 
        | (_,_) => raise Fail "not right types substract")
    | typeof (env,S.Mul(n1,n2)) = (case ((typeof (env,n1)), (typeof (env,n2))) of 
          (T.Nat, T.Nat) => T.Nat
        | (_,_) => raise Fail "not right types mul")
    | typeof (env,S.Pow(n1,n2)) = (case ((typeof (env,n1)), (typeof (env,n2))) of 
          (T.Nat, T.Nat) => T.Nat
        | (_,_) => raise Fail "not right types pow")
    | typeof (env,S.Less(n1,n2)) = (case ((typeof (env,n1)), (typeof (env,n2))) of 
          (T.Nat, T.Nat) => T.Bool
        | (_,_) => raise Fail "not right types less")
    | typeof (env,S.Greater(n1,n2)) = (case ((typeof (env,n1)),(typeof (env,n2))) of 
          (T.Nat,T.Nat) => T.Bool
        | (_,_) => raise Fail "not right types greater")
    | typeof (env,S.LessEq(n1,n2)) = (case ((typeof (env,n1)),(typeof (env,n2))) of 
          (T.Nat,T.Nat) => T.Bool
        | (_,_) => raise Fail "not right types lesseq")
    | typeof (env,S.GreaterEq(n1,n2)) = (case ((typeof (env,n1)),(typeof (env,n2))) of 
          (T.Nat,T.Nat) => T.Bool
        | (_,_) => raise Fail "not right types GreaterEq")
    | typeof (env,S.Not b1) = (case (typeof (env,b1)) of 
          T.Bool => T.Bool
        | _ => raise Fail "not right Not")
    | typeof (env,S.And (t1,t2)) = (case ((typeof (env,t1)), (typeof (env,t2))) of 
          (T.Bool,T.Bool) => T.Bool
        | (_,_) => raise Fail "not right And" )
    | typeof (env,S.Or (t1,t2)) = (case ((typeof (env,t1)), (typeof (env,t2))) of 
          (T.Bool,T.Bool) => T.Bool
        | (_,_) => raise Fail "not right Or" )
    | typeof (env,S.Xor (t1,t2)) = (case ((typeof (env,t1)), (typeof (env,t2))) of 
          (T.Bool,T.Bool) => T.Bool
        | (_,_) => raise Fail "not right XOr" )
    | typeof (env,S.Cond(c,t1,t2)) = (case ((typeof (env,c),typeof (env,t1),typeof (env,t2) )) of
          (T.Bool, t1',t2') => if t1' = t2' then 
              t2'
            else 
              raise Fail "not right cond t1 != t2"
        | (_,_,_) => raise Fail "not right c not bool") 
    | typeof (env,S.Eq(t1,t2)) = (case ((typeof (env,t1)), (typeof (env,t2))) of
          (T.Nat,T.Nat) => T.Bool 
        | _ => raise Fail "not right eq t1!=t2")
    | typeof (env,S.Pair(t1,t2)) = (case ((typeof (env,t1)), (typeof (env,t2))) of 
          (t1',t2') => T.Product(t1',t2'))
    | typeof (env,S.First t1) = (case (typeof (env,t1)) of 
          T.Product(t1',_)=>t1'
        | _ => raise Fail "not right First")
    | typeof (env,S.Second t1) = (case (typeof (env,t1)) of 
          T.Product(_,t2')=>t2'
        | _ => raise Fail "not right First")
    | typeof (env,S.Var str) = (case TypeEnv.lookup(env,str) of 
          SOME t => t
        | NONE => raise Fail "not right, var no def")
    | typeof (env,S.Let(str,t1,t2)) = let
        val newEnv = TypeEnv.extend(env,str,(typeof (env,t1)))
      in
        typeof(newEnv,t2)
      end
    
    
end
