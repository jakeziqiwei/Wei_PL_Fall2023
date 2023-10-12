structure Test = struct

        structure T = Token
        structure A = AST

        fun scan () =
                        let
                                val _ = Check.expect (Scan.next [#"Z"], SOME (T.Z, []),"test")
                                (* write more scan tests here *)
                                val _ = Check.expect (Scan.scan "ZZZZ", [T.Z,T.Z,T.Z,T.Z], "scan0")
                                val _ = Check.expect (Scan.scan "[T> T]", [T.LBrack,T.T,T.GreaterThan,T.T,T.RBrack], "scan1")
                                val _ = Check.expect (Scan.scan "[? Z : F]", [T.LBrack,T.QuestionMark,T.Z,T.Colon,T.F,T.RBrack], "scan2")
                                val _ = Check.expect (Scan.scan "&&", [T.DoubleAmpersand], "scan3")
                                val _ = Check.expect (Scan.scan "||", [T.DoublePipe], "scan4") 
                                val _ = Check.expect (Scan.scan "[T> T]", [T.LBrack,T.T,T.GreaterThan,T.T,T.RBrack], "scan5")
                                val _ = Check.expect (Scan.scan "[T    > T]", [T.LBrack,T.T,T.GreaterThan,T.T,T.RBrack], "scan6")
                                val _ = Check.expect (Scan.scan "[T - F]", [T.LBrack,T.T,T.Minus,T.F,T.RBrack], "scan7") 
                                val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
                                val _ = Check.exn (fn () => Scan.scan "Z~Z", "badScan01")
                                val _ = Check.exn (fn () => Scan.scan "T& &||F", "badScan02")
                                val _ = Check.expect (Scan.scan "Z&&F", [T.Z,T.DoubleAmpersand,T.F], "scan8")
                                val _ = Check.expect (Scan.scan "T&&||F", [T.T,T.DoubleAmpersand, T.DoublePipe,T.F], "scan9")          
                        in
                                TextIO.print "scan tests done\n"
                        end

        fun parse () =
                        let
                                val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.Z,T.Plus,T.Z,T.RBrack], A.Add(A.Zero,A.Zero), "parse1")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.Z,T.Minus,T.Z,T.RBrack], A.Subtract(A.Zero,A.Zero), "parse2")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.Z,T.GreaterThan,T.Z,T.RBrack], A.Greater(A.Zero,A.Zero), "parse3")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.Z,T.LessThan,T.Z,T.RBrack], A.Less(A.Zero,A.Zero), "parse4")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.Z,T.DoubleAmpersand,T.Z,T.RBrack], A.And(A.Zero,A.Zero), "parse5")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.Z,T.DoublePipe,T.Z,T.RBrack], A.Or(A.Zero,A.Zero), "parse6")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.T, T.QuestionMark, T.Z,T.Colon,T.Z,T.RBrack], A.Cond(A.True,A.Zero,A.Zero), "parse7")
                                val _ = Check.expect (Parse.parse [T.S,T.S,T.Z], A.Succ(A.Succ(A.Zero)), "parse8")
                                val _ = Check.expect (Parse.parse [T.P,T.P,T.Z], A.Pred(A.Pred(A.Zero)), "parse9")
                                (* Complex ones *)
                                val _ = Check.expect (Parse.parse [T.LBrack,T.Z,T.Plus,T.LBrack,T.F,T.Minus,T.T,T.RBrack,T.RBrack], A.Add(A.Zero,A.Subtract(A.False,A.True)), "parse10")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.LBrack,T.F,T.Minus,T.T,T.RBrack,T.Plus,T.S,T.Z,T.RBrack], A.Add(A.Subtract(A.False,A.True), (A.Succ A.Zero)), "parse11")
                                val _ = Check.expect (Parse.parse [T.LBrack,T.LBrack,T.Z,T.DoublePipe,T.T,T.RBrack,T.QuestionMark,T.LBrack,T.LBrack,T.F,T.Minus,T.T,T.RBrack,T.Plus,T.Z,T.RBrack,T.Colon,T.LBrack,T.F,T.GreaterThan,T.S,T.Z,T.RBrack,T.RBrack], 
                                                A.Cond(A.Or(A.Zero,A.True),A.Add(A.Subtract(A.False,A.True),A.Zero),A.Greater(A.False,A.Succ(A.Zero))), "parse12")
                                val _ = Check.expect (Parse.parse [Token.LBrack,Token.T,Token.QuestionMark,Token.LBrack,Token.T,Token.DoubleAmpersand,Token.F,Token.RBrack,Token.Colon,Token.LBrack,Token.S,Token.S,Token.F,Token.LessThan,Token.S,Token.Z,Token.RBrack,Token.RBrack], 
                                                A.Cond(A.True, A.And(A.True,A.False), A.Less((A.Succ(A.Succ AST.False)),(A.Succ AST.Zero))), "parse13")
                                val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
                                val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
                                val _ = Check.exn (fn () => Parse.parse [T.Z, T.Plus, T.Z], "badParse1")
                                val _ = Check.exn (fn () => Parse.parse [T.QuestionMark], "badParse2")
                                val _ = Check.exn (fn () => Parse.parse [T.RBrack], "badParse3")
                                val _ = Check.exn (fn () => Parse.parse [T.Z,T.Z], "badParse4")
                                val _ = Check.exn (fn () => Parse.parse [T.LBrack,T.Z,T.Z,T.RBrack], "badParse5")
                                (* write more parse tests here *)
                        in
                                TextIO.print "parse tests done\n"
                        end

        fun eval () =
                        let
                                val _ = Check.expect (Eval.eval A.Zero, [A.Zero], "eval0")
                                val _ = Check.expect(Eval.eval (A.Add (A.Zero,A.Zero)), [A.Add (A.Zero,A.Zero), A.Zero], "eval1")
                                val _ = Check.expect(Eval.eval (A.Succ (A.And (A.True, A.False)))
                                                , [A.Succ (A.And (A.True, A.False)), A.Succ (A.False)], "eval2")
                                val _ = Check.expect(Eval.eval (A.Pred A.Zero), [A.Pred A.Zero, A.Zero], "eval3")
                                val _ = Check.expect(Eval.eval (A.Pred (A.Succ (A.Zero)))
                                                , [(A.Pred (A.Succ (A.Zero))), A.Zero], "eval4")
                                val _ = Check.expect(Eval.eval (A.Pred (A.Pred (A.Zero)))
                                                ,[A.Pred (A.Pred A.Zero),A.Pred A.Zero,A.Zero], "eval5")
                                val _ = Check.expect(Eval.eval (A.Add (A.Succ (A.Succ (A.Zero)), A.Zero))
                                                , [A.Add (A.Succ (A.Succ A.Zero),A.Zero),A.Add (A.Succ A.Zero,A.Succ A.Zero),
                                                        A.Add (A.Zero,A.Succ (A.Succ A.Zero)),A.Succ (A.Succ A.Zero)]
                                                , "eval6")
                                val _ = Check.expect(Eval.eval (A.Add ((A.Succ A.Zero), A.True))
                                                , [(A.Add ((A.Succ A.Zero), A.True)),A.Add (A.Zero,A.Succ A.True),
                                                        (A.Succ A.True)]
                                                , "eval7")
                                val _ = Check.expect(Eval.eval (A.Subtract (A.Succ (A.Succ (A.Zero)), A.Zero))
                                                , [(A.Subtract (A.Succ (A.Succ (A.Zero)), A.Zero)), A.Succ (A.Succ A.Zero)]
                                                , "eval8")
                                val _ = Check.expect(Eval.eval (A.Subtract (A.Zero, A.Succ (A.Succ (A.Zero))))
                                                , [(A.Subtract (A.Zero, A.Succ (A.Succ (A.Zero)))), A.Zero]
                                                , "eval9")
      
                                val _ = Check.expect(Eval.eval (A.Subtract (A.Succ A.Zero, A.Succ A.Zero))
                                                , [(A.Subtract (A.Succ A.Zero, A.Succ A.Zero)), A.Subtract (A.Zero, A.Zero), A.Zero]
                                                , "eval10")
                                val _ = Check.expect(Eval.eval (A.Less (A.Zero, A.Zero))
                                                , [(A.Less (A.Zero, A.Zero)), A.False]
                                                , "eval11")
                                val _ = Check.expect(Eval.eval (A.Less (A.Zero, A.Succ (A.Succ A.Zero)))
                                                , [(A.Less (A.Zero, A.Succ (A.Succ A.Zero))), A.True]
                                                , "eval12")
                                val _ = Check.expect(Eval.eval (A.Less (A.Succ A.Zero, A.Zero))
                                                , [(A.Less (A.Succ A.Zero, A.Zero)), A.False]
                                                , "eval13")
                                val _ = Check.expect(Eval.eval (A.Less (A.Succ A.Zero, A.Succ A.Zero))
                                                , [(A.Less (A.Succ A.Zero, A.Succ A.Zero)), A.Less (A.Zero, A.Zero), A.False]
                                                , "eval14")
                                val _ = Check.expect(Eval.eval (A.Less (A.Succ A.Zero, A.Succ A.True))
                                                , [(A.Less (A.Succ A.Zero, A.Succ A.True))]
                                                , "eval15")
                                val _ = Check.expect(Eval.eval (A.Less (A.And (A.True, A.False), A.Succ A.Zero))
                                                , [(A.Less (A.And (A.True, A.False), A.Succ A.Zero)), (A.Less (A.False, A.Succ A.Zero))]
                                                , "eval16")   
                                val _ = Check.expect(Eval.eval (A.Greater (A.Zero, A.Succ A.Zero))
                                                , [A.Greater (A.Zero,A.Succ A.Zero), A.False]
                                                , "eval17")  
                                val _ = Check.expect(Eval.eval (A.Greater (A.Succ A.Zero, A.Zero))
                                                , [(A.Greater (A.Succ A.Zero, A.Zero)), A.True]
                                                , "eval18") 
                                val _ = Check.expect(Eval.eval (A.Greater (A.Succ A.Zero, A.And(A.True, A.True)))
                                                , [(A.Greater (A.Succ A.Zero, A.And(A.True, A.True))), (A.Greater (A.Succ A.Zero, A.True))]
                                                , "eval19")   
                                val _ = Check.expect(Eval.eval (A.And (A.True, A.And(A.True, A.False)))
                                                , [(A.And (A.True, A.And(A.True, A.False))), A.And(A.True, A.False), A.False]
                                                , "eval20")
                                val _ = Check.expect(Eval.eval (A.And (A.False, A.And(A.True, A.False)))
                                                , [(A.And (A.False, A.And(A.True, A.False))), A.False]
                                                , "eval20") 
                                val _ = Check.expect(Eval.eval (A.And (A.And (A.True, A.True), A.And (A.True, A.Zero)))
                                                , [A.And (A.And (A.True,A.True),A.And (A.True,A.Zero)),A.And (A.True,A.And (A.True,A.Zero)),
                                                        A.And (A.True,A.Zero),A.Zero]
                                                , "eval21") 
                                val _ = Check.expect(Eval.eval (A.Or (A.And (A.True, A.True), A.Or (A.True, A.Zero)))
                                                , [A.Or (A.And (A.True,A.True),A.Or (A.True,A.Zero)),A.Or (A.True,A.Or (A.True,A.Zero)), A.True]
                                                , "eval22")
                                val _ = Check.expect(Eval.eval (A.Cond (A.True, A.Or (A.False, A.False), A.Zero))
                                                , [(A.Cond (A.True, A.Or (A.False, A.False), A.Zero)), A.Or (A.False, A.False), A.False]
                                                , "eval23")
                                val _ = Check.expect(Eval.eval (A.Cond (A.False, A.Or (A.False, A.False), A.And (A.True, A.True)))
                                                , [(A.Cond (A.False, A.Or (A.False, A.False), A.And (A.True, A.True))), A.And (A.True, A.True), A.True]
                                                , "eval24")
                                val _ = Check.expect(Eval.eval (AST.Cond (AST.And (AST.False, AST.True), AST.Or (AST.False, AST.False), AST.And(AST.True, AST.True)))
                                                , [(A.Cond ((A.And (A.False, A.True)), A.Or (A.False, A.False), A.And (A.True, A.True)))
                                                        , (A.Cond (A.False, A.Or (A.False, A.False), A.And (A.True, A.True)))
                                                        , A.And (A.True, A.True)
                                                        , A.True]
                                                , "eval25")
                                val _ = Check.expect(Eval.eval (A.Cond(A.Greater(A.Succ A.Zero, A.Succ A.Zero), A.Zero, A.Or(A.True, A.False))),
                                                [(A.Cond(A.Greater(A.Succ A.Zero, A.Succ A.Zero), A.Zero, A.Or(A.True, A.False))), 
                                                        (A.Cond(A.Greater( A.Zero, A.Zero), A.Zero, A.Or(A.True, A.False))),
                                                        (A.Cond(A.False, A.Zero, A.Or(A.True, A.False))),
                                                        A.Or(A.True, A.False),
                                                        A.True],
                                                "eval26")
                                val _ = Check.expect(Eval.eval(A.Pred(A.False)), [A.Pred(A.False)], "eval28")
                                (* write more eval tests here *)
                        in
                                TextIO.print "eval tests done\n"
                        end

        fun compile () =
                        let
                                val _ = Check.expect (Compile.code "SZ", [A.Succ A.Zero], "compile0")
                                (* write more eval tests here *)
                        in
                                TextIO.print ("compile tests done\n")
                        end
      
        fun all () =
                        let
                                val _ = scan ()
                                val _ = parse ()
                                val _ = eval ()
                                val _ = compile ()
                        in
                                TextIO.print "all tests done\n"
                        end
      
end
