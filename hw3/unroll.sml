structure Unroll : sig

  val unroll : Sweetl.prog -> Sweetl.term
  (*  check if there are any abbrev not in set*)
  val abbrevCheck : (string * Sweetl.term) list -> string list -> bool 
end = struct

  (* [("ab",Sweetl.Lam ("a",Sweetl.Var "b")),("cd",Sweetl.Lam ("c",Sweetl.Var "d"))]*)
  (* [("x",Sweetl.App (Sweetl.Abbr "ab", Sweetl.Abbr "ab")),("ab",(Sweetl.var "a"))]*)

  fun abbrevCheck abbrevs set = 
      let
        fun termCheck t set = case t of 
              Sweetl.Abbr(s) => if (List.exists (fn x => x = s) set) then 
                  true 
                else 
                  false
            | Sweetl.App(t1,t2) => (termCheck t1 set) andalso (termCheck t2 set)
            | Sweetl.Lam(s,t1) => termCheck t1 set
            | _ => true 
      in
        case abbrevs of
          ((s,t):: rest) => if (termCheck t set) then 
              abbrevCheck rest (s :: set)
            else 
              false 
        | [] => true 
      end  

  fun unroll (Sweetl.Prog(abbrs, main)) = 
      if abbrevCheck abbrs [] then 
        case main of 
          (Sweetl.Abbr s) => (case (List.find (fn (x,y) => x = s) abbrs) of 
                SOME (_,t1) => unroll (Sweetl.Prog(abbrs,t1))
              | NONE => raise Fail "hmm interesting, main used a abbr not defined")
        | Sweetl.App (ap1,ap2) => Sweetl.App(unroll (Sweetl.Prog(abbrs,ap1)), unroll (Sweetl.Prog(abbrs,ap2)))
        | Sweetl.Lam(s,l1) => Sweetl.Lam(s, unroll(Sweetl.Prog(abbrs,l1)))
        | _ => main 
      else 
        raise Fail "Abbreviation was not well defined"


end
