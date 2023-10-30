structure CBV : sig

  val step : ULC.term -> ULC.term option
  val isV : ULC.term -> bool

end = struct

  fun isV (ULC.Var x) = false 
    | isV (ULC.App(t1,t2)) = false 
    | isV (ULC.Lam (s,t)) = true 


  fun step (ULC.Var x) = NONE
    | step (ULC.Lam(x,s)) = NONE
    | step (ULC.App(t1,t2)) = (case (isV t1, isV t2) of 
          (false,_) => (case step t1 of 
                SOME t1' => SOME (ULC.App(t1',t2))
              | NONE => NONE)
        | (true, false) => (case step t2 of 
              SOME t2' => SOME (ULC.App(t1,t2'))
            | NONE => NONE) 
        | (true,true) => (case t1 of 
              ULC.Lam (str,term) => SOME (Subst.subst(str,t2,term))
            | _ => NONE))
end


