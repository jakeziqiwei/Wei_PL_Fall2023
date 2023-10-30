structure Lazy : sig

  val step : ULC.term -> ULC.term option

end = struct
		  
  fun step (ULC.Var x) = NONE
    | step (ULC.Lam(x,t)) =  NONE
    | step (ULC.App(t1,t2)) = case t1 of 
        (ULC.Lam(x,t)) => SOME(Subst.subst(x,t2,t))
      | term => case step term of 
          SOME t1' => SOME (ULC.App(t1',t2))
        | NONE => NONE

end
