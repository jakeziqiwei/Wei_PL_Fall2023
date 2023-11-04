structure Desugar : sig

  val desugar : Sugary.term -> ULC.term
  val int_to_ULC: int -> ULC.term

end = struct

  structure S = Sugary
  structure U = ULC
  structure T = Type


  fun int_to_ULC num = if num = 1 then 
        ULC.App(ULC.Var "s", ULC.Var"z")
      else 
        ULC.App(ULC.Var "s",int_to_ULC (num-1))

  fun desugar (S.Var str) = U.Var str
    | desugar (S.True) = U.Lam("t",U.Lam("f",U.Var "t"))
    | desugar (S.False) = U.Lam("t",U.Lam("f",U.Var "f"))
    | desugar (S.Nat num) = if num = 0 then 
        U.Lam("s", U.Lam("z",U.Var"z"))
      else 
        U.Lam("s", U.Lam("z",(int_to_ULC num)))
    | desugar (S.Unit) = U.Lam("s", U.Lam("z",U.Var"z"))
    | desugar (S.And (c,b)) = 
      let
        val churchAnd = U.Lam("b",U.Lam("c", U.App(U.App(U.Var "b", U.Var "c"), desugar(S.False))))
      in
        U.App(U.App(churchAnd,desugar c), desugar(b))
      end
    | desugar (S.Not b) = 
      let
        val churchNot = U.Lam("b", U.App(U.App(U.Var "b", desugar(S.False)), desugar(S.True)))
      in
        U.App(churchNot,desugar(b))
      end
    | desugar (S.Or (b1,b2)) = 
      let
        val churchOr = U.Lam("b", U.Lam("c", U.App(U.App(U.Var "b",desugar(S.True)),U.Var "c")))
      in
        U.App(U.App(churchOr,desugar(b1)),desugar(b2))
      end
    | desugar(S.Add (n1,n2)) = 
      let
        val churchAdd = U.Lam ("m", U.Lam ("n", U.Lam ("s",U.Lam ("z",U.App (U.App (U.Var "m",U.Var "s"),U.App (U.App (U.Var "n",U.Var "s"),U.Var "z"))))))
      in
        U.App(U.App(churchAdd,desugar(n1)),desugar(n2))
      end
    | desugar (S.Mul(n1,n2)) =  
      let
        val churchMul = U.Lam("m", U.Lam("n", U.Lam ("s", U.App(U.Var "m",U.App (U.Var "n", U.Var"s")))))
      in
        U.App(U.App(churchMul,(desugar n1)), (desugar n2))
      end
    | desugar (S.Pair (p1,p2)) = 
      let
        val churchpair = U.Lam ("f",U.Lam ("s",U.Lam ("b",U.App (U.App (U.Var "b",U.Var "f"),U.Var "s"))))
      in
        U.App(U.App(churchpair,(desugar p1)),desugar p2)
      end
    |desugar (S.First t1) = 
      let
        val churchFirst = U.Lam("p",U.App(U.Var "p",desugar(S.True)))
      in
        U.App(churchFirst,desugar t1)

      end
    | desugar (S.Second t1) = 
      let
        val churchSecond = U.Lam("p",U.App(U.Var "p",desugar(S.False)))
      in
        U.App(churchSecond, (desugar t1))
      end
    | desugar (S.Subtract(n1,n2)) = 
      let
        val churchzz = desugar (S.Pair(S.Nat 0, S.Nat 0))
        val churchPlus = U.Lam ("m", U.Lam ("n", U.Lam ("s",U.Lam ("z",U.App (U.App (U.Var "m",U.Var "s"),U.App (U.App (U.Var "n",U.Var "s"),U.Var "z"))))))
        val churchPair = U.Lam ("f",U.Lam ("s",U.Lam ("b",U.App (U.App (U.Var "b",U.Var "f"),U.Var "s"))))
        val churchSnd = U.Lam("p",U.App(U.Var "p",desugar(S.False)))
        val churchFst = U.Lam("p",U.App(U.Var "p",desugar(S.True)))
        val churchSS = U.Lam("p", 
            U.App(
              (U.App(churchPair,
                  U.App(churchSnd, U.Var "p")) 
              ), 
              U.App(
                (U.App (churchPlus, desugar(S.Nat 1))),
                U.App(churchSnd, U.Var "p"))))
        val churchPrd = U.Lam("m", U.App(churchFst, U.App(U.App(U.Var "m",churchSS),churchzz)))
        val churchSubtract = U.Lam("m",U.Lam("n", U.App(U.App(U.Var "n",churchPrd), U.Var "m")))
      in
        U.App(U.App(churchSubtract, desugar(n1)), desugar (n2))
      end
    | desugar (S.Cond (t1,t2,t3)) = let

        val churchTest = U.Lam("l", U.Lam("m", U.Lam ("n", U.App(U.App(U.Var "l",U.Var "m"), U.Var "n"))))
        (* :cond = [b [x [y ((b x) y)]]]; *)
      in
        U.App(U.App(U.App(churchTest, desugar(t1)), desugar(t2)),desugar t3)   
      end
    | desugar (S.Let (str,t1,t2)) = U.App(U.Lam(str,desugar(t2)),desugar(t1))
    | desugar (S.Eq (t1,t2)) = 
      let
        val (typ1,typ2) = (TypeCheck.typeof(TypeEnv.empty,t1), TypeCheck.typeof(TypeEnv.empty,t2))
      in
        if (typ1 = T.Bool andalso typ2 = T.Bool) then 
          desugar (S.Not(S.Xor(t1,t2)))
        else 
          if (typ1 = T.Nat andalso typ2 = T.Nat) then 
            desugar(S.And(S.LessEq(t1,t2),S.LessEq(t2,t1)))
          else 
            if (typ1 = T.Unit andalso typ2 = T.Unit) then 
              desugar(S.True)
            else 
              case (t1,t2) of 
                (S.Pair(ap1,ap2), S.Pair(ap3,ap4)) => desugar(S.And(S.Eq(ap1,ap3),S.Eq(ap2,ap4)))
              | _ => raise Fail "wut"
      end
    | desugar (S.Xor(t1,t2)) = 
      let
        val churchNot = U.Lam("b", U.App(U.App(U.Var "b", desugar(S.False)), desugar(S.True)))
        val churchXor = U.Lam("a", U.Lam("b", U.App(U.App(U.Var "a", U.App(churchNot, U.Var"b")), U.Var "b")))
      in
        U.App(U.App(churchXor,desugar(t1)),desugar t2)
      
      end
    | desugar (S.Pow(t1,t2)) = 
      let
        val churchMul = U.Lam("m", U.Lam("n", U.Lam ("s", U.App(U.Var "m",U.App (U.Var "n", U.Var"s")))))
        val churchPow = U.Lam("m", U.Lam("n", U.App(U.App(U.Var "n", U.App(churchMul, U.Var "m")),int_to_ULC(1))))
      in
        U.App(U.App(churchPow,desugar(t1)), desugar(t2))
      end

    | desugar (S.LessEq (t1,t2)) = 
      let
        val churchIsZero = U.Lam ("m", U.App((U.App(U.Var "m", U.Lam("x", desugar(S.False)))),desugar(S.True)))
        val temp = desugar(S.Subtract(t1,t2))
      in
        U.App(churchIsZero,temp)
      end
    | desugar (S.GreaterEq (t1,t2)) = desugar(S.LessEq(t2,t1))
    | desugar (S.Greater (t1,t2)) = desugar(S.Not(S.LessEq(t1,t2)))
    | desugar (S.Less (t1,t2)) = desugar(S.Greater(t2,t1))

    
end
