structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST

  fun next [] = NONE

    | next (Token.S :: toks) = (case next toks of 
          SOME(asts, tail) => SOME (AST.Succ(asts), tail)
        | _ => raise Fail "nothing after succ")
    | next (Token.P :: toks) = (case next toks of 
          SOME(asts, tail) => SOME (AST.Pred (asts), tail)
        | _ => raise Fail "nothing after pred")
    | next (Token.LBrack:: toks) = (case (next toks) of 
          SOME (t1, Token.Plus::rest) => (case (next rest) of 
                SOME(t2, Token.RBrack::tail) => SOME (AST.Add(t1,t2),tail)
              | _ => raise Fail "Error1")
        | SOME (t1, Token.Minus::rest) => (case (next rest) of 
              SOME(t2, Token.RBrack::tail) => SOME (AST.Subtract(t1,t2),tail)
            | _ => raise Fail "Error2")
        | SOME (t1, Token.LessThan::rest) => (case (next rest) of 
              SOME(t2, Token.RBrack::tail) => SOME (AST.Less(t1,t2),tail)
            | _ => raise Fail "Error2")
        | SOME (t1, Token.GreaterThan::rest) => (case (next rest) of 
              SOME(t2,Token.RBrack::tail) => SOME (AST.Greater(t1,t2),tail)
            | _ => raise Fail "error3")
        | SOME (t1, Token.DoubleAmpersand::rest) => (case (next rest) of 
              SOME(t2,Token.RBrack::tail) => SOME(AST.And(t1,t2),tail)
            | _ => raise Fail "error4")
        | SOME (t1, Token.DoublePipe::rest) => (case (next rest) of 
              SOME(t2,Token.RBrack::tail) => SOME(AST.Or(t1,t2),tail)
            | _ => raise Fail "error5")
        | SOME (t1, Token.QuestionMark::rest) => (case (next rest) of
              SOME(t2,Token.Colon::tail) => (case (next tail) of 
                    SOME(t3, Token.RBrack::list) => SOME(AST.Cond(t1,t2,t3),list)
                  | _ => raise Fail "error6")
            | _ => raise Fail "error7")
        | SOME (t1,Token.RBrack :: rest) => SOME(t1,rest)
        | _ => raise Fail "error8")
    | next (Token.Z :: toks) = SOME (AST.Zero, toks)
    | next (Token.T :: toks) = SOME (AST.True, toks)
    | next (Token.F :: toks) = SOME (AST.False, toks)
    | next (Token.Plus :: toks) = raise Fail "ERror"
    | next (Token.LessThan :: toks) = raise Fail "ERror"
    | next (Token.GreaterThan :: toks) = raise Fail "ERror"
    | next (Token.DoublePipe :: toks) = raise Fail "ERror"
    | next (Token.DoubleAmpersand :: toks) = raise Fail "ERror"
    | next (Token.QuestionMark :: toks) = raise Fail "ERror"
    | next (Token.Colon :: toks) = raise Fail "ERror"
    | next _ = raise Fail "ERror"

                                  

  fun parse toks = 
      case(next toks) of 
        SOME(t1, []) => t1 
      | SOME (t1, _) => raise Fail "there are terms left"
      | None => raise Fail "empty program"

		     
end
