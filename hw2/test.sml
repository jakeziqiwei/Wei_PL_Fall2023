structure Test = struct

  structure T = Token
  structure S = Sugary
  structure D = Desugared
  structure E = Eval
		  
  fun scan () =
      let
        val _ = Check.expect (Scan.scan "312", [T.Nat 312], "scan12")
        val _ = Check.expect (Scan.scan ">12", [T.GreaterThan,T.Nat 12], "scan12")
        val _ = Check.expect (Scan.scan ">12", [T.GreaterThan,T.Nat 12], "scan12")
        val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
        val _ = Check.expect (Scan.scan ">=<(12", [T.GreaterEq,T.LessThan,T.LParen,T.Nat 12], "scan12")
        val _ = Check.expect (Scan.scan "12>=<(", [T.Nat 12,T.GreaterEq,T.LessThan,T.LParen], "scan12")
      in
        TextIO.print "scan tests done\n"
      end

  fun parse () =
      let
        val _ = Check.expect (Parse.parse [T.Nat 12], S.Nat 12, "parse12")
        val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
        (* write more parse tests here *)
      in
        TextIO.print "parse tests done\n"
      end

  fun typ () =
      let
        val _ = Check.expect (TypeCheck.typeof (S.Unit), Type.Unit, "type0")
        val _ = Check.expect (TypeCheck.typeof (S.Add(S.Nat 12, S.Nat 23)), Type.Nat, "type1")
        val _ = Check.expect (TypeCheck.typeof (Sugary.Cond ((Sugary.Not Sugary.True),Sugary.True,Sugary.True)), Type.Bool, "type2") 
        val _ = Check.expect (TypeCheck.typeof (S.Subtract(S.Nat 12, S.Nat 32)), Type.Nat, "type3")
        val _ = Check.expect (TypeCheck.typeof (S.Less(S.Nat 12, S.Nat 32)), Type.Bool, "type4") 
        val _ = Check.expect (TypeCheck.typeof (S.Nat 12), Type.Nat, "type5")  
        val _ = Check.expect (TypeCheck.typeof (S.LessEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23)) )), Type.Bool, "type6") 
        val _ = Check.expect (TypeCheck.typeof (S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12)), Type.Bool, "type7") 
        val _ = Check.expect (TypeCheck.typeof (S.GreaterEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12)), Type.Bool, "type8") 
        val _ = Check.expect (TypeCheck.typeof (S.GreaterEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23)) )), Type.Bool, "type9") 
        val _ = Check.expect (TypeCheck.typeof (S.Not(S.GreaterEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23)) ))), Type.Bool, "type10") 
        val _ = Check.expect (TypeCheck.typeof (S.And(S.GreaterEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23))),S.True)), Type.Bool, "type11") 
        val _ = Check.expect (TypeCheck.typeof (S.Or (S.True,(S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12)))), Type.Bool, "type12") 
        val _ = Check.expect (TypeCheck.typeof (S.Xor (S.True,(S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12)))), Type.Bool, "type13") 
        val _ = Check.expect (TypeCheck.typeof (S.Cond(S.True, S.False, S.False)), Type.Bool, "type14") 
        val _ = Check.expect (TypeCheck.typeof (S.Cond(S.True, (S.Not(S.GreaterEq(S.Nat 12,(S.Add(S.Nat 12, S.Nat 23)) ))), S.False)), Type.Bool, "type15")
        val _ = Check.expect (TypeCheck.typeof (S.Cond(S.True, S.False,(S.Or (S.True,(S.LessEq((S.Add(S.Nat 12, S.Nat 23)),S.Nat 12)))) )), Type.Bool, "type16")
        val _ = Check.expect (TypeCheck.typeof (S.Eq(S.Nat 12, S.Nat 23)), Type.Bool, "type17")
        val _ = Check.expect (TypeCheck.typeof (S.Eq(S.False, S.True)), Type.Bool, "type18")
        val _ = Check.expect (TypeCheck.typeof (S.Pair(S.Nat 2, S.Nat 32)), (Type.Product(Type.Nat,Type.Nat)), "type19")
        val _ = Check.expect (TypeCheck.typeof (S.Pair(S.True, S.Nat 53)), (Type.Product(Type.Bool,Type.Nat)), "type20")
        val _ = Check.expect (TypeCheck.typeof (S.Pair((S.Less(S.Nat 12, S.Nat 32)), S.Nat 55)), (Type.Product(Type.Bool,Type.Nat)), "type21")
        val _ = Check.expect (TypeCheck.typeof (S.First(S.Pair((S.Less(S.Nat 12, S.Nat 32)), S.Nat 67))), Type.Bool, "type22")
        val _ = Check.expect (TypeCheck.typeof (S.First(S.Pair(S.Nat 21, S.False))), Type.Nat, "type23")
        val _ = Check.expect (TypeCheck.typeof (S.Second(S.Pair((S.Less(S.Nat 12, S.Nat 32)), S.Nat 432))), Type.Nat, "type24")
        val _ = Check.expect (TypeCheck.typeof (S.Second(S.Pair(S.Nat 32, S.False))), Type.Bool, "type25")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Add(S.Nat 12, S.True)), "badScan00")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Subtract(S.Nat 12, S.Pair(S.Nat 12, S.Nat 23))), "badScan01")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Subtract(S.Nat 12, S.False)), "badScan2")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Less(S.Nat 12, S.False)), "badScan03")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Not((S.Add(S.Nat 12, S.Nat 23)))), "badScan04")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.And(S.Nat 23,S.Nat 32)), "badScan0")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Or(S.Nat 23,(S.Add(S.Nat 12, S.Nat 23)))), "badScan05")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Xor(S.Nat 23,(S.Subtract(S.Nat 12, S.Nat 23)))), "badScan06")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Cond(S.True, S.False, S.Nat 12)), "badScan07")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Cond(S.Nat 12, S.False, S.Nat 12)), "badScan08")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.First(S.Unit)), "badScan9")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.First (S.Add(S.Nat 12, S.Nat 23))), "badScan10")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Second(S.Unit)), "badScan11")
        val _ = Check.exn (fn () => TypeCheck.typeof (S.Second (S.Add(S.Nat 12, S.Nat 23))), "badScan12")
      in
        TextIO.print "type tests done\n"
      end

  fun desugar () =
      let
        val _ = Check.expect (Desugar.desugar (S.Nat 0), D.Zero, "desugar0")
        val _ = Check.expect (Desugar.desugar (S.True), (D.Succ D.Zero), "desguar1")
        val _ = Check.expect (Desugar.desugar (S.False), D.Zero, "desguar2")
        val _ = Check.expect (Desugar.desugar (S.Nat 2), (D.Succ (D.Succ D.Zero)), "desguar3")
        val _ = Check.expect (Desugar.desugar (S.Add (S.Nat 0, S.Nat 0)), (D.Add(D.Zero,D.Zero)) , "desguar4")
        val _ = Check.expect (Desugar.desugar (S.Add (S.Nat 0, S.Nat 2)), (D.Add(D.Zero,(D.Succ (D.Succ D.Zero)))) , "desguar5")
        val _ = Check.expect (Desugar.desugar (S.Add (S.Nat 2, S.Nat 0)), (D.Add((D.Succ (D.Succ D.Zero)),D.Zero)) , "desguar6")
        val _ = Check.expect (Desugar.desugar (S.Subtract ((S.Add ((S.Nat 0), S.Nat 0)),S.Nat 0)),D.Subtract(D.Add(D.Zero,D.Zero),D.Zero), "desguar7")
        val _ = Check.expect (Desugar.desugar (S.Subtract ((S.Add ((S.Nat 2), S.Nat 0)),S.Nat 0)),D.Subtract(D.Add(D.Succ (D.Succ D.Zero),D.Zero),D.Zero), "desguar8")
        val _ = Check.expect (Desugar.desugar (S.Less (S.Nat 0,S.Nat 0)), (D.Less(D.Zero,D.Zero)), "desguar9")
        val _ = Check.expect (Desugar.desugar (S.Less ((S.Add ((S.Nat 0), S.Nat 0)),S.Nat 0)),D.Less(D.Add(D.Zero,D.Zero),D.Zero), "desguar10")
        val _ = Check.expect (Desugar.desugar (S.Eq (S.Nat 3,S.False)), (D.Eq((D.Succ (D.Succ (D.Succ D.Zero))),D.Zero)), "desguar11")
        val _ = Check.expect (Desugar.desugar (S.Eq ((S.Add ((S.Nat 0), S.Nat 0)),(S.Add ((S.Nat 0), S.Nat 0)))), (D.Eq(D.Add(D.Zero,D.Zero),D.Add(D.Zero,D.Zero))), "desguar12")
        val _ = Check.expect (Desugar.desugar (S.Cond (S.True, S.False,S.True)), D.Cond(D.Succ D.Zero, D.Zero,D.Succ D.Zero), "desguar13")
        val _ = Check.expect (Desugar.desugar (S.Cond (S.True, (S.Add (S.Nat 0, S.Nat 0)),S.True)), D.Cond((D.Succ D.Zero), D.Add(D.Zero, D.Zero),D.Succ D.Zero), "desguar14")
        val _ = Check.expect (Desugar.desugar (S.Cond (S.False, S.False, (S.Add (S.Nat 0, S.Nat 0)))), (D.Cond(D.Zero, D.Zero, D.Add(D.Zero, D.Zero))), "desguar15")
        val _ = Check.expect (Desugar.desugar (S.Pair (S.True,S.False)), (D.Pair(D.Succ D.Zero, D.Zero)), "desguar16")
        val _ = Check.expect (Desugar.desugar (S.Pair(S.Subtract (S.Nat 0, S.Nat 0),S.Nat 0)), (D.Pair(D.Subtract(D.Zero,D.Zero), D.Zero)), "desguar17") 
        val _ = Check.expect (Desugar.desugar (S.First(S.Pair(S.False,S.True))), (D.First(D.Zero)), "desguar18") 
        val _ = Check.expect (Desugar.desugar (S.Second(S.Pair(S.True,(S.Add (S.Nat 0, S.Nat 0))))), (D.Second (D.Add(D.Zero,D.Zero))), "desguar19") 


      in
        TextIO.print "desugar tests done\n"
      end



        
  fun eval () =
      let
        
        val _ = Check.expect (Eval.result D.Zero, Eval.Value D.Zero, "eval0")
        (* write more eval tests here *)
      in
        TextIO.print "eval tests done\n"
      end

  fun compile () =
      let
        fun value typ program result =
            Check.expect (Compile.code program, (E.Value result, typ), "compile"^program)
        val natval = value Type.Nat
        val boolval = value Type.Bool
        val _ = natval "0" D.Zero 
        (* write more compile tests here *)
      in
        TextIO.print ("compile tests done\n")
      end
      
  fun all () =
      let
        val _ = scan ()
        val _ = parse ()
        val _ = typ ()
        val _ = desugar ()
        val _ = eval ()
        val _ = compile ()
      in
        TextIO.print "all tests done\n"
      end
      
end


