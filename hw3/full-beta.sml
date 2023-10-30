structure FullBeta : sig

  val step : ULC.term -> ULC.term option

end = struct


  fun step (ULC.Var x) = NONE
    | step (ULC.Lam(x,t1)) = (case step t1 of 
          SOME t1' => SOME (ULC.Lam(x,t1'))
        | NONE => NONE)
    | step (ULC.App(t1,t2)) = (case (t1,t2) of 
          (ULC.Lam(x,term), _) => SOME(Subst.subst(x,t2,term))
        |(x,y) => (case step x of 
              SOME x' => SOME(ULC.App(x',t2))
            |NONE => (case step y of 
                  SOME y' => SOME (ULC.App(t1,y'))
                | NONE => NONE)))
          
end
 