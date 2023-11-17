structure Subst : sig

  val subst : string * L23RR.term * L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  (* note: We will do without VarSet/FV this time. *)
  (* This is because free variables are effectively banished by the typechecker. *)
  (* That is, we shouldn't have them when we get to evaluation. *)
		  
  fun subst (x, s, t) = case t of 
        L.Int n => L.Int n
      | L.True => L.True 
      | L.False => L.False 
      | L.Unit => L.Unit
      | L.Var str => if str = x then 
          s 
        else 
          t
      | L.Lam(str,typ,t2) => if str = x then 
          L.Lam(str,typ,t2) 
        else 
          L.Lam(str,typ,subst(x,s,t2))
      | L.Fix n => L.Fix(subst(x,s,n))
      | L.App(t1,t2) => L.App(subst(x,s,t1), subst(x,s,t2))
      | L.Let(str,t1,t2) => if str = x then 
          L.Let(str,subst(x,s,t1),t2)
        else 
          L.Let(str,subst(x,s,t1),subst(x,s,t2))
      | L.Cond(t1,t2,t3) => L.Cond(subst(x,s,t1),subst(x,s,t2),subst(x,s,t3))
      | L.Add(t1,t2) => L.Add(subst(x,s,t1),subst(x,s,t2))
      | L.Sub(t1,t2) => L.Sub(subst(x,s,t1),subst(x,s,t2))
      | L.Mul(t1,t2) => L.Mul(subst(x,s,t1),subst(x,s,t2))
      | L.Eq(t1,t2) => L.Eq(subst(x,s,t1),subst(x,s,t2))
      | L.LessThan(t1,t2) => L.LessThan(subst(x,s,t1),subst(x,s,t2))
      | L.Not(t1) => L.Not(subst(x,s,t1))
      | L.Record(lst) => L.Record(List.map (fn (t:string, tm:L.term) => (t, subst(x,s,tm))) lst)
      | L.Select(t,lst) => L.Select(t,subst(x,s,lst))

end
