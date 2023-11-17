structure RecordCheck : sig

  (* check for pairwise distinct labels at all levels of record expressions and record types *)
  (* also, reject empty records if you encounter them *)

  (* raise an exception if the term doesn't pass the check *)

  (* otherwise, return the term as is *)

  val check : L23RR.term -> L23RR.term
  val recordCheck : (string * L23RR.term) list -> string list -> bool
  val recordTypeCheck : (string * Type.typ) list -> string list -> bool
	    
end = struct

  structure L = L23RR
  structure T = Type



  fun recordCheck [] set = true 
    | recordCheck ((str,t1) ::rest) set = case t1 of 
        L.Record(t2) => if (List.exists (fn x => x = str) set) orelse ((recordCheck t2 [])=false) then 
            false 
          else 
            (recordCheck rest (str :: set))
      | _ => if (List.exists (fn x => x = str) set) then 
          false 
        else 
          (recordCheck rest (str :: set))


  fun recordTypeCheck [] set = true 
    | recordTypeCheck ((str,typ) ::rest) set = case typ of 
        T.Record(t2) => if (List.exists (fn x => x = str) set) orelse ((recordTypeCheck t2 [])=false) then 
            false 
          else 
            (recordTypeCheck rest (str :: set))
      | _ => if (List.exists (fn x => x = str) set) then 
          false 
        else 
          (recordTypeCheck rest (str :: set))



  fun check (L.Int n) =  (L.Int n)
    | check (L.True) = L.True 
    | check (L.False) = L.False 
    | check (L.Unit) = L.Unit 
    | check (L.Var str) = L.Var str 
    | check (L.Lam (str, typ, t)) = (case typ of 
          T.Record(lst) => if (recordTypeCheck lst [] = true) then 
              L.Lam(str,typ,check(t))
            else 
              raise Fail "record type not correct"
        | _ => L.Lam(str,typ,check(t)))
    | check (L.App(t1,t2)) = (L.App(check t1, check t2))
    | check (L.Fix t1) = (L.Fix (check t1)) 
    | check (L.Let (str,t2,t3)) = (L.Let (str, check t2, check t3))
    | check (L.Cond(t1,t2,t3)) = (L.Cond(check t1,check t2,check t3))
    | check (L.Add(t1,t2)) = L.Add(check t1,check t2)
    | check (L.Sub (t1,t2)) = L.Sub(check t1,check t2)
    | check (L.Mul(t1,t2)) = L.Mul(check t1,check t2)
    | check (L.Eq (t1,t2)) = L.Eq(check t1,check t2)
    | check (L.LessThan (t1,t2)) = L.LessThan(check t1,check t2)
    | check (L.Not t1) = L.Not(check t1)
    | check (L.Record t1) = (case recordCheck t1 [] of
          true => (L.Record t1) 
        | false => raise Fail "Record not correct")
    | check (L.Select (str,t1)) = L.Select(str,check t1)

end
