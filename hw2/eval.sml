structure Eval : sig

  val isV  : Desugared.term -> bool
  val step : Desugared.term -> Desugared.term option
  val eval : Desugared.term -> Desugared.term list

  datatype norm
    = Value of Desugared.term
  | Stuck of Desugared.term	    

  val result : Desugared.term -> norm
      
end = struct

  datatype norm
    = Value of Desugared.term
  | Stuck of Desugared.term

  structure D = Desugared

  fun isV D.Zero = true 
    | isV (D.Succ t) = isV t 
    | isV (D.Pair (t1,t2)) = (isV t1 andalso isV t2)
    | isV _ = false
	
  fun step D.Zero = NONE
    | step (D.Succ t1) = (case step t1 of 
          SOME (t1') => SOME(D.Succ t1')
        | NONE => NONE)
    | step (D.Add(t1,t2)) = (if (isV t1) then 
          case t1 of 
            (D.Succ t1') => SOME(D.Add (t1',D.Succ t2))
          | (D.Zero) => SOME (t2)
          | (D.Pair( t1', t2')) => NONE
          | _ => raise Fail "Wut?"
        else 
          (case (step t1) of 
              SOME t1' => SOME (D.Add(t1',t2))
            | NONE => NONE))
    | step (D.Subtract(t1,t2)) = (if (t1 = D.Zero) then 
          if isV(t2) then
            SOME(t1)
          else
            if (isV t1) then 
              case step(t2) of 
                SOME(t2') => SOME(D.Subtract(t1,t2'))
              | NONE => NONE
            else 
              (case (step t1) of 
                  SOME t1' => SOME (D.Subtract(t1',t2))
                | NONE => NONE)
        else
          if (t2 = D.Zero) then 
            if (isV t1) then 
              SOME(t1)
            else 
              (case (step t1) of 
                  SOME t1' => SOME (D.Subtract(t1',t2))
                | NONE => NONE)
          else 
            if ((isV t1) andalso (isV t2)) then 
              case (t1,t2) of 
                (D.Succ(nv1), D.Succ(nv2)) => SOME (D.Subtract(nv1,nv2))
              | (D.Pair(_,_), _) => NONE 
              | (_,D.Pair(_,_)) => NONE 
              | (_,_) => raise Fail "wut??"
            else 
              if (isV t1) then
                case (step t2) of 
                  SOME(t2') => SOME(D.Subtract(t1,t2'))
                | NONE => NONE
              else
                (case (step t1) of 
                    SOME t1' => SOME (D.Subtract(t1',t2))
                  | NONE => NONE))
    | step (D.Less (t1,t2)) = (case (t1,t2) of 
          (D.Zero, D.Zero) => SOME (D.Zero)
        | (D.Zero, t2) => (if isV t2 then 
              case t2 of 
                D.Succ t2' => SOME (D.Succ D.Zero)
              | D.Pair(_,_) => NONE
              |  _ => raise Fail "????"
            else
              ( case step t2 of 
                  (SOME t2') => SOME (D.Less(t1,t2'))
                | NONE => NONE))
        | (t1, D.Zero) => (if isV t1 then 
              SOME(D.Zero)
            else 
              ( case step t1 of 
                  (SOME t1') => SOME (D.Less(t1',t2))
                | NONE => NONE))
        | (t1,t2) => (if (isV(t1) andalso isV(t2)) then
              case (t1,t2) of 
                (D.Succ(nv1), D.Succ(nv2)) => SOME (D.Less(nv1,nv2))
              | (D.Pair _, _) => NONE 
              | (_, D.Pair _) => NONE
              | (_,_) => raise Fail "wut??"  
            else
              if isV t1 then
                case step t2 of
                  SOME(t2') => SOME(D.Less(t1,t2'))
                | NONE => NONE                                                    
              else
                case step t1 of
                  SOME(t1') => SOME(D.Less(t1',t2))
                | NONE => NONE))
    | step (D.Eq (t1,t2)) = (case (t1,t2) of 
          (D.Zero, D.Zero) => SOME (D.Succ D.Zero)
        | (D.Zero, t2) => (if (isV t2) then 
              (case t2 of 
                  (D.Succ t2') => SOME (D.Zero)
                | (D.Pair (_,_)) => NONE
                | _ => raise Fail "wut")
            else 
              (case (step t2) of 
                  SOME (t2') => SOME (D.Eq(t1,t2'))
                | NONE => NONE))
        | (t1, D.Zero) => if (isV t1) then 
            (case t1 of 
                (D.Succ t1') => SOME(D.Zero)
              | (D.Pair(_,_)) => NONE
              | _ => NONE)
          else 
            (case step t1 of 
                (SOME t1') => SOME(D.Eq (t1',t2))
              | NONE => NONE)
        | (D.Pair (t11,t12), D.Pair (t21,t22)) => (if (isV t11 andalso isV t12 andalso isV t21 andalso isV t21) then 
              SOME (D.Cond(D.Eq(t11,t21), D.Eq(t12,t22), D.Zero))
            else 
              (if (isV (D.Pair (t11,t12))) then 
                  case step (D.Pair (t11,t12)) of 
                    SOME (t2') => SOME(D.Eq(t1,t2'))
                  | NONE => NONE
                else 
                  case step (D.Pair (t11,t21)) of 
                    SOME (t2') => SOME(D.Eq(t1,t2'))
                  | NONE => NONE))
        | (t1,t2) => (if (isV t1 andalso isV t2) then 
              (case (t1,t2) of 
                  (D.Succ v1, D.Succ v2) => SOME(D.Eq(v1,v2))
                | (D.Pair _, _) => NONE 
                | (_, D.Pair _) => NONE
                | (_,_) => raise Fail "wut??" )
            else
              if (not (isV t1)) then 
                case step t1 of 
                  SOME (t1') => SOME(D.Eq(t1',t2))
                | NONE => NONE
              else 
                case step t2 of 
                  SOME (t2') => SOME(D.Eq(t1,t2'))
                |NONE => NONE))
    | step (D.Cond(t1,t2,t3)) = (case t1 of 
          (D.Succ D.Zero) => SOME(t2)
        | (D.Zero) => SOME(t3)
        | v1 => if (not (isV v1)) then 
            case step v1 of 
              SOME (v1') => SOME (D.Cond(v1',t2,t3))
            | NONE => NONE
          else 
            raise Fail "wut?")
    
    | step (D.Pair (t1,t2)) = (if isV t1 then 
          case (step t2) of 
            SOME (t2') => SOME (D.Pair(t1,t2'))
          |NONE => NONE
        else 
          case step t1 of 
            SOME(t1') => SOME (D.Pair(t1',t2))
          | NONE => NONE)
    | step (D.First t1) = (case t1 of 
          (D.Pair (p1, p2)) => if (isV p1 andalso isV p2) then 
              SOME p1 
            else 
              (case step (D.Pair (p1, p2)) of 
                  SOME(p') => SOME (D.First(p'))
                | NONE => NONE )
        | mp => if isV mp then 
            raise Fail "not a pair and value"
          else 
            case step mp of 
              SOME mp' => SOME (D.First mp')
            | NONE => NONE)

    | step (D.Second t1) = (case t1 of 
          (D.Pair (p1, p2)) => (if (isV p1 andalso isV p2) then 
                SOME p2 
              else 
                (case step (D.Pair (p1, p2)) of 
                    SOME(p') => SOME(D.Second(p'))
                  | NONE => NONE ))
        | mp => if isV mp then 
            raise Fail "not a pair and value"
          else 
            (case step mp of 
                SOME mp' => SOME(D.Second mp')
              | NONE => NONE))  
    

  fun eval t =
      let
        fun lp t =
            (case step t
              of SOME t' => t :: lp t'
              | NONE => [t])
      in
        lp t
      end		    

  fun result code = 
      let
        val last = List.last (eval code)
      in
        if (isV last) then 
          Value last
        else 
          Stuck last
      end
end

