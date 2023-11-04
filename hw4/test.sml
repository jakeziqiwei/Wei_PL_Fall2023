structure Test = struct

  (* tests here... *)
  structure S = Sugary
  structure T = Type
  structure U = ULC


  fun typecheck () =
      let
        val _ = Check.expect (TypeCheck.typeof ((TypeEnv.empty,S.Unit)), Type.Unit, "type0")
        (* val _ = Check.expect (TypeCheck.typeof (([],(S.Add(S.Nat 12, S.Nat 23)))), Type.Nat, "type1")
        val _ = Check.expect (TypeCheck.typeof (([],Sugary.Cond ((Sugary.Not Sugary.True),Sugary.True,Sugary.True))), Type.Bool, "type2") 
        val _ = Check.expect (TypeCheck.typeof (([],S.Subtract(S.Nat 12, S.Nat 32))), Type.Nat, "type3")
        val _ = Check.expect (TypeCheck.typeof (([],S.Less(S.Nat 12, S.Nat 32))), Type.Bool, "type4") 
        val _ = Check.expect (TypeCheck.typeof ([],S.Nat 12), Type.Nat, "type5")  
        val _ = Check.expect (TypeCheck.typeof (([],S.Mul(S.Nat 12, S.Nat 32))), Type.Nat, "type3")
        val _ = Check.expect (TypeCheck.typeof (([],S.Pow(S.Nat 12, S.Nat 32))), Type.Nat, "type3")
        val _ = Check.expect (TypeCheck.typeof (([],S.LessEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23))))), Type.Bool, "type6") 
        val _ = Check.expect (TypeCheck.typeof (([],S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12))), Type.Bool, "type7") 
        val _ = Check.expect (TypeCheck.typeof (([],S.GreaterEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12))), Type.Bool, "type8") 
        val _ = Check.expect (TypeCheck.typeof (([],S.GreaterEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23)) ))), Type.Bool, "type9") 
        val _ = Check.expect (TypeCheck.typeof (([],S.Not(S.GreaterEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23)))))), Type.Bool, "type10") 
        val _ = Check.expect (TypeCheck.typeof (([],S.And(S.GreaterEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23))),S.True))), Type.Bool, "type11") 
        val _ = Check.expect (TypeCheck.typeof (([],S.Or (S.True,(S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12))))), Type.Bool, "type12") 
        val _ = Check.expect (TypeCheck.typeof (([],S.Xor (S.True,(S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12))))), Type.Bool, "type13") 
        val _ = Check.expect (TypeCheck.typeof (([],S.Cond(S.True, S.False, S.False))), Type.Bool, "type14") 
        val _ = Check.expect (TypeCheck.typeof (([],S.Cond(S.True, (S.Not(S.GreaterEq (S.Nat 12, (S.Add(S.Nat 12, S.Nat 23))))), S.False))), Type.Bool, "type15")
        val _ = Check.expect (TypeCheck.typeof (([],S.Cond(S.True, S.False,(S.Or (S.True,(S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12))))) )), Type.Bool, "type16")
        val _ = Check.expect (TypeCheck.typeof ([],(S.Eq(S.Nat 12, S.Nat 23))), Type.Bool, "type17")
        val _ = Check.expect (TypeCheck.typeof (([],S.Eq(S.False, S.True))), Type.Bool, "type18")
        val _ = Check.expect (TypeCheck.typeof ([],(S.Pair(S.Nat 2, S.Nat 32))), (Type.Product(Type.Nat,Type.Nat)), "type19")
        val _ = Check.expect (TypeCheck.typeof ([],(S.Pair(S.True, S.Nat 53))), (Type.Product(Type.Bool,Type.Nat)), "type20")
        val _ = Check.expect (TypeCheck.typeof ([],(S.Pair((S.Less(S.Nat 12, S.Nat 32)), S.Nat 55))), (Type.Product(Type.Bool,Type.Nat)), "type21")
        val _ = Check.expect (TypeCheck.typeof ([],(S.First(S.Pair((S.Less(S.Nat 12, S.Nat 32)), S.Nat 67)))), Type.Bool, "type22")
        val _ = Check.expect (TypeCheck.typeof ([],(S.First(S.Pair(S.Nat 21, S.False)))), Type.Nat, "type23")
        val _ = Check.expect (TypeCheck.typeof ([],(S.Second(S.Pair((S.Less(S.Nat 12, S.Nat 32)), S.Nat 432)))), Type.Nat, "type24")
        val _ = Check.expect (TypeCheck.typeof ([],(S.Second(S.Pair(S.Nat 32, S.False)))), Type.Bool, "type25") 
        val _ = Check.expect (TypeCheck.typeof ([("a",T.Bool)],(S.Var "a")), T.Bool, "Type26")
        val _ = Check.expect (TypeCheck.typeof ([], (S.Let("a",(S.Nat 2),(S.Add(S.Var "a", S.Nat 2))))), T.Nat, "Type27")
        val _ = Check.expect (TypeCheck.typeof ([], (S.Let("x",(S.Nat 0),(S.Let("x",S.True,S.Not(S.Var("x"))))))), T.Bool, "Type28")
        val _ = Check.exn (fn () => TypeCheck.typeof ([],S.Add(S.Nat 2, S.True)), "badScan00")
        val _ = Check.exn (fn () => TypeCheck.typeof ([],(S.Var "a")), "badScan01")
        val _ = Check.exn (fn () => TypeCheck.typeof ([], (S.Let("x",(S.True),(S.Let("x",(S.Nat 0),S.Not(S.Var("x"))))))), "badScan02") *)

      in
        TextIO.print "typeof tests done\n"
      end

  fun desugar () =
      let
        val _ = Check.expect(Desugar.desugar (S.Var "a"), U.Var "a", "desugar 1")
        val _ = Check.expect(Desugar.desugar (S.True), U.Lam("t", U.Lam("f", U.Var "t")), "desugar 2")  

      in
        TextIO.print "desugar tests done\n"
      end
  fun all () =
      let
        val _ = typecheck()
        val _ = desugar()


      in
        TextIO.print "all tests done\n"
      end
      
end
