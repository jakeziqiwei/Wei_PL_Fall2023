structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct
		  
  fun fv (ULC.Var s)= VarSet.ins(s,VarSet.empty)
    | fv (ULC.App (ap1, ap2)) = VarSet.union((fv ap1), (fv ap2))
    | fv (ULC.Lam (s, ap1)) = VarSet.rem(s,fv(ap1))

  fun subst (x, s, t1) = case t1 of 
        (ULC.Var str) => (if str = x then
              s
            else 
              t1)
      | (ULC.App(ap1,ap2)) => (ULC.App((subst (x, s, ap1)),(subst (x, s, ap2))))
      | (ULC.Lam (y,t2)) => if x = y then 
          ULC.Lam(y,t2) 
        else 
          if VarSet.mem(y,fv(s)) then 
            let
              val new = Fresh.var()
            in
              subst(x, s , ULC.Lam(new, subst(y,ULC.Var new,t2)))
            end      
          else 
            ULC.Lam(y , subst(x,s,t2))
end
