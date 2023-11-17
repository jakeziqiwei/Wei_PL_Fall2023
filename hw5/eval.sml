structure Eval : sig

  val eval : L23RR.term -> L23RR.term
  val isV : L23RR.term -> bool
	    
end = struct

  structure L = L23RR

  (* v ::= T
    | F
    | ()
    | n  // integer constants
    | [lam x tau t]
    | [~l v ... ~l v]  // records *)

  fun isV (L.True) = true 
    | isV (L.False) = true 
    | isV (L.Unit) = true 
    | isV (L.Lam(str,typ,t)) = true 
    | isV (L.Record(lst)) = let
        val recList = List.map (fn (_, term) => isV term) lst
      in
        not (List.exists (fn (x : bool) => x = false) recList)
      end
    | isV (L.Int n) = true 
    | isV _ = false 

  fun eval (L.Int n) = (L.Int n)
    | eval (L.True) = L.True 
    | eval (L.False) = L.False 
    | eval (L.Unit) = L.Unit
    | eval (L.Var str) = raise Fail "stand alone var"
    | eval (L.Lam (str,typ,t1)) = L.Lam (str,typ,t1)
    | eval (L.App(t1,t2)) = (case (eval t1, eval t2) of 
          (L.Lam(x,tau1,t11),v2) => if isV v2 then 
              let
                val v3 = eval(Subst.subst(x,v2,t11))
              in
                if isV v3 then 
                  v3
                else 
                  raise Fail "Lam sub t2 into t1 is not a value"
              end
            else 
              raise Fail "not able to take big step on t2' since its not a value"
        | (_,_) => raise Fail "t1 is not a lam")
    | eval (L.Fix t1) = (case (eval t1) of 
          L.Lam(f,tau1,t11) => let
              val v1 = eval(Subst.subst(f,L.Fix(L.Lam(f,tau1,t11)),t11))
            in
              if isV v1 then 
                v1
              else 
                raise Fail "t1 is not a value"
            end
        | _ => raise Fail "t1 eval not to a lambda term")
    | eval (L.Let (str,t1,t2)) = (case eval t1 of 
          v1 => if isV v1 then 
              let
                val v2 = eval(Subst.subst(str,v1,t2))
              in
                v2
              end
            else 
              raise Fail "t1 does not step into v1 for let")
    | eval (L.Cond (t1,t2,t3)) = (case (eval t1) of 
          L.True => 
            let
              val v2 = eval t2
            in
              if isV v2 then 
                v2 
              else 
                raise Fail "t2 does not step into a value"
            end
        | L.False =>     let
            val v3 = eval t3
          in
            if isV v3 then 
              v3 
            else 
              raise Fail "t3 does not step into a value"
          end
        | _ => raise Fail "cond t1 is not bool")
    | eval (L.Add(t1,t2)) = (case (eval t1, eval t2) of 
          (L.Int n1, L.Int n2) => L.Int (n1+n2)
        | (_,_) => raise Fail "t1 and t2 does not eval into intergers")
    | eval (L.Sub(t1,t2)) = (case (eval t1, eval t2) of 
          (L.Int n1, L.Int n2) => L.Int (n1-n2)
        | (_,_) => raise Fail "t1 and t2 does not eval into intergers")
    | eval (L.Mul(t1,t2)) = (case (eval t1, eval t2) of 
          (L.Int n1, L.Int n2) => L.Int (n1*n2)
        | (_,_) => raise Fail "t1 and t2 does not eval into intergers")
    | eval (L.Eq(t1,t2)) = (case (eval t1, eval t2) of 
          (L.Int n1, L.Int n2) => if n1 = n2 then 
              L.True 
            else 
              L.False
        | (_,_) => raise Fail "t1 and t2 does not eval into intergers")
    | eval (L.LessThan (t1,t2)) = (case (eval t1, eval t2) of 
          (L.Int n1, L.Int n2) => if n1 < n2 then 
              L.True 
            else 
              L.False
        | (_,_) => raise Fail "t1 and t2 does not eval into intergers")
    | eval (L.Not t1 )= (case eval t1 of 
          L.True => L.False 
        | L.False => L.True 
        | _ => raise Fail "t1 is not a bool for not")
    | eval (L.Record lst) = let
        val recList = List.map (fn (t, term) => (t,eval term)) lst
        val boollist = List.map (fn (_, term) => isV term) recList
      in
        if (List.exists (fn (x : bool) => x = false) boollist) then 
          raise Fail "not all terms in record can step into value"
        else 
          L.Record(recList)
      end
    | eval (L.Select (str,t1)) = case t1 of 
        L.Record (lst) =>  (case (List.find (fn (x:string,y:L.term) => x = str) lst) of
              SOME (str',t1') => if isV t1' then 
                  t1'
                else 
                  eval(t1')
            | NONE => raise Fail "cant find the term in record")
      | _ => raise Fail "select not followed by record"
    

  
		 
end
