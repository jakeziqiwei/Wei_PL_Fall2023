structure Desugar : sig

      val desugar : Sugary.term -> Desugared.term

end = struct

      structure S = Sugary
      structure D = Desugared

      fun desugar S.True = (D.Succ D.Zero)
            | desugar S.False = (D.Zero)
            | desugar S.Unit = (D.Zero)
            | desugar (S.Nat t1) = if (t1 = 0) then 
                        (D.Zero) 
                  else 
                        (case desugar (S.Nat (t1-1)) of 
                                    t1' => D.Succ(t1'))
            | desugar (S.Add (t1,t2)) = (case (desugar t1, desugar t2) of 
                              (t1',t2') => D.Add(t1',t2'))
            | desugar (S.Subtract (t1,t2)) = (case (desugar t1, desugar t2) of 
                              (t1',t2') => D.Subtract(t1',t2'))
            | desugar (S.Less (t1,t2)) = (case (desugar t1, desugar t2) of 
                              (t1',t2') => D.Less(t1',t2'))
            | desugar (S.Eq (t1,t2)) = (case (desugar t1, desugar t2) of 
                              (t1',t2') => D.Eq(t1',t2'))
            | desugar (S.Cond (t1,t2,t3)) = (case (desugar t1, desugar t2, desugar t3) of 
                              (t1',t2',t3') => D.Cond(t1',t2',t3'))
            | desugar (S.Pair (t1,t2)) = (case (desugar t1, desugar t2) of 
                              (t1',t2') => D.Pair(t1',t2'))
            | desugar (S.First t1) = (case (desugar t1) of 
                              D.Pair(t1',_) => D.First t1'
                        | _ => raise Fail "First Needs to be followed by a pair/pair expression")
            | desugar (S.Second t1) = (case (desugar t1) of 
                              D.Pair(_,t2') => D.Second t2'
                        | _ => raise Fail "Second Needs to be followed by a pair/pair expression")
            | desugar (S.Greater (t1,t2)) = desugar (S.Less (t2,t1))
            | desugar (S.Not t1) = desugar (S.Cond(t1, S.False, S.True))
            | desugar (S.And (t1,t2)) = desugar (S.Cond (t1, t2, S.False))
            | desugar (S.Or (t1,t2)) = desugar (S.Cond (t1, S.True, t2))
            | desugar (S.Xor (t1,t2)) = desugar(S.Cond(S.Eq(t1,t2), S.False, S.True))
            | desugar (S.GreaterEq (t1,t2)) = desugar(S.Or(S.Greater(t1,t2),S.Eq(t1,t2)))
            | desugar (S.LessEq (t1,t2)) = desugar(S.Or (S.Less(t1,t2), S.Eq(t1,t2)))

end
