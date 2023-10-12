structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV A.Zero = true 
    | isNV (A.Succ t1) = isNV(t1)
    | isNV _ = false 

  fun isV a = 
      case a of 
        A.True => true 
      |A.False => true 
      |A.Zero => true 
      |A.Succ(t1) => isNV(a)
      | _ => false 
		 
  fun step (A.Zero) = NONE
    | step(A.True) = NONE
    | step(A.False) = NONE
    | step (A.Succ(t1)) = (case (step t1) of 
          SOME (t1') => SOME (A.Succ t1')
        | NONE => NONE)
    | step (A.Pred A.Zero) = SOME(A.Zero)
    | step (A.Pred t1) = (if isNV t1 then 
          (case t1 of 
              A.Succ nv1 => SOME(nv1)
            |A.Zero => raise Fail "what???"
            | _ => raise Fail "what??")
        else 
          case (step t1) of 
            SOME t1' => SOME (A.Pred t1')
          |NONE => NONE )
    | step (A.Add(t1, t2)) =  if (isNV t1) then 
        (case (t1) of 
            (A.Succ(nv)) => SOME (A.Add(nv,(A.Succ t2)))
          | (A.Zero) => SOME (t2)
          | _ => raise Fail "error")
      else
        (case (step t1) of 
            SOME t1' => SOME (A.Add(t1',t2))
          | NONE => NONE)
    | step (A.Subtract(t1,t2)) = (if (t1 = A.Zero) then 
          if isNV(t2) then
            SOME(t1)
          else
            if (isV t1) then 
              case step(t2) of 
                SOME(t2') => SOME(A.Subtract(t1,t2'))
              | NONE => NONE
            else 
              (case (step t1) of 
                  SOME t1' => SOME (A.Subtract(t1',t2))
                | NONE => NONE)
        else
          if (t2 = A.Zero) then 
            if (isNV t1) then 
              SOME(t1)
            else 
              (case (step t1) of 
                  SOME t1' => SOME (A.Subtract(t1',t2))
                | NONE => NONE)
          else 
            if ((isNV t1) andalso (isNV t2)) then 
              case (t1,t2) of 
                (A.Succ(nv1), A.Succ(nv2)) => SOME (A.Subtract(nv1,nv2))
              | (_,_) => raise Fail "wut??"
            else 
              if (isV t1) then
                case (step t2) of 
                  SOME(t2') => SOME(A.Subtract(t1,t2'))
                | NONE => NONE
              else
                (case (step t1) of 
                    SOME t1' => SOME (A.Subtract(t1',t2))
                  | NONE => NONE))
    | step (A.Greater (t1,t2)) = (if (t1 = A.Zero andalso (isNV t2)) then
          SOME(A.False)
        else
          if (t2 = A.Zero) then 
            if (isNV t1) then 
              case (t1) of 
                A.Succ(t1') => SOME(A.True)
              | _ => SOME (A.Greater(t1,t2))
            else 
              case step t1 of 
                SOME(t1') => SOME (A.Greater(t1',t2))
              | NONE => NONE
          else
            if ((isNV t1) andalso (isNV t2)) then 
              case (t1,t2) of 
                (A.Succ(nv1), A.Succ(nv2)) => SOME (A.Greater(nv1,nv2))
              | (_,_) => raise Fail "wut??"   
            else
              if (isV t1) then 
                case (step t2) of 
                  SOME (t2') => SOME (A.Greater(t1,t2'))
                | NONE => NONE
              else 
                case (step t1) of 
                  SOME(t1') => SOME (A.Greater (t1',t2))
                | NONE => NONE)
    | step (A.Less (t1,t2)) = (case (t1,t2) of 
          (A.Zero, A.Zero) => SOME(A.False)
        | (A.Zero, t2) => (if (isNV t2) then 
              case t2 of 
                A.Succ(t2') => SOME(A.True)
              | A.Zero => raise Fail "???"
              | _ => raise Fail "????"
            else
              case step t2 of
                SOME(t2') => SOME(A.Less(t1,t2'))
              | NONE => NONE)
        | (t1, A.Zero) => (if (isNV t1) then 
              case t1 of 
                A.Succ(t1') => SOME(A.False)
              | A.Zero => raise Fail "???"
              | _ => raise Fail "????"
            else
              case step t1 of 
                SOME(t1') => SOME(A.Less(t1',t2)) 
              | NONE => NONE
          )
        | (t1,t2) => (if (isNV(t1) andalso isNV(t2)) then
              case (t1,t2) of 
                (A.Succ(nv1), A.Succ(nv2)) => SOME (A.Less(nv1,nv2))
              | (_,_) => raise Fail "wut??"  
            else
              if isV t1 then
                case step t2 of
                  SOME(t2') => SOME(A.Less(t1,t2'))
                | NONE => NONE                                                    
              else
                case step t1 of
                  SOME(t1') => SOME(A.Less(t1',t2))
                | NONE => NONE))
    | step (A.And(t1,t2)) = if (t1 = A.True) then 
        SOME (t2)
      else
        if (t1 = A.False) then 
          SOME (A.False)
        else 
          (case (step t1) of 
              SOME (t1') => SOME(A.And(t1',t2))
            | NONE => NONE)
    | step (A.Or(t1,t2)) = if (t1 = A.True) then 
        SOME (A.True)
      else
        if (t1 = A.False) then 
          SOME (t2)
        else 
          (case (step t1) of 
              SOME (t1') => SOME(A.Or(t1',t2))
            | NONE => NONE)
    | step (A.Cond(A.True,t2,_)) = SOME t2
    | step (A.Cond(A.False,_,t3)) = SOME t3
    | step (A.Cond(t1,t2,t3)) = (case (step t1) of 
          SOME t1' => SOME(A.Cond(t1',t2,t3))
        | NONE => NONE)


  fun eval a = 
      let 
        fun lp asts = 
            case (step asts) of 
              SOME (ast) => [ast]@lp ast
            | NONE => []
      in
        a :: lp a 
      end  
end
