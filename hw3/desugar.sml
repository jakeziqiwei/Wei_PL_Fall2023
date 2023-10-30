structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term

end = struct

  (*  1  = ULC.App(ULC.Var "s",ULC.Var"z")

2 = ULC.app("s",ULC.App(ULC.Var "s",ULC.Var"z"))
*)

  fun int_to_ULC num = if num = 1 then 
        ULC.App(ULC.Var "s", ULC.Var"z")
      else 
        ULC.App(ULC.Var "s",int_to_ULC (num-1))

  fun desugar (Sweetl.Var s) = (ULC.Var s) 
    | desugar (Sweetl.Abbr s) = raise Fail "unroll failed"
    | desugar (Sweetl.App (t1, t2)) = ULC.App((desugar t1), (desugar t2))
    | desugar (Sweetl.Lam(s,t1)) = ULC.Lam(s,desugar t1)
    | desugar (Sweetl.Tru) = ULC.Lam("t",ULC.Lam("f",ULC.Var "t"))
    | desugar (Sweetl.Fls) = ULC.Lam("t",ULC.Lam("f",ULC.Var "f"))
    | desugar (Sweetl.ID s) = ULC.Lam(s,ULC.Var s)
    | desugar (Sweetl.Nat num) = if num = 0 then 
        ULC.Lam("s", ULC.Lam("z",ULC.Var"z"))
      else 
        ULC.Lam("s", ULC.Lam("z",(int_to_ULC num)))


end
