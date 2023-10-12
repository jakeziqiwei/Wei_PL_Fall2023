structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun next [] = NONE 
    | next (#"[" :: cs) = SOME (Token.LBrack, cs)
    | next (#"]" :: cs) = SOME (Token.RBrack, cs)
    | next (#"Z" :: cs) = SOME (Token.Z, cs)
    | next (#"S" :: cs) = SOME (Token.S, cs)
    | next (#"T" :: cs) = SOME (Token.T, cs)
    | next (#"F" :: cs) = SOME (Token.F, cs)
    | next (#"P" :: cs) = SOME (Token.P, cs)
    | next (#"+" :: cs) = SOME (Token.Plus, cs)
    | next (#"-" :: cs) = SOME (Token.Minus, cs)
    | next (#"<" :: cs) = SOME (Token.LessThan, cs)
    | next (#">" :: cs) = SOME (Token.GreaterThan, cs)
    | next (#"&" :: #"&" :: cs) = SOME (Token.DoubleAmpersand, cs)
    | next (#"|" :: #"|" :: cs) = SOME (Token.DoublePipe, cs)
    | next (#"?" :: cs) = SOME (Token.QuestionMark, cs)
    | next (#":" :: cs) = SOME (Token.Colon, cs)  
    | next (c :: cs) = if Char.isSpace c then 
        next cs 
      else
        raise Fail "not tokenizable"

  fun scan code =
      let 
        fun lp [] = []
          | lp chars =
            case (next chars) of 
              SOME(tok, cs) => tok :: lp cs
            | NONE => []
      in 
        lp (explode code)
      end
end
