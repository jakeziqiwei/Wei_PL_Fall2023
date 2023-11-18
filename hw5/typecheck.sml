structure TypeCheck : sig

  (* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

  (* for commonSupertype, use the s function from the PDF *)
  (* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option
  val typeof : L23RR.term -> Type.typ
  (* type check with env*)
  val envTypeCheck : ( TypeEnv.env * L23RR.term) -> Type.typ
  val permAndWidth : ((string * Type.typ) list * (string * Type.typ) list) -> bool
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv


  fun permAndWidth (_,[]) = true 
    | permAndWidth (lst1,(s,typ)::rest) = (case (List.exists (fn (x : string,y:Type.typ) => x = s andalso y = typ) lst1) of 
          true => permAndWidth(lst1,rest)
        | false => false)

  (* check if the first type is a sub type of the second one*)
  fun subty (T.Int,T.Int) = true 
    | subty (T.Bool, T.Bool) = true 
    | subty (T.Unit, T.Unit) = true  
    | subty (T.Function (s1,s2), T.Function (t1,t2)) = if subty(t1,s1) andalso subty(s2,t2) then 
        true 
      else 
        false 
    | subty(T.Record lst1, T.Record lst2) = let
        fun depthRecord(_,[]) = true
          | depthRecord(lst1, (str,t)::rest) = case (List.find (fn (s:string,t:Type.typ) => s = str) lst1) of 
              SOME (str',t') => if subty(t',t) then 
                  depthRecord(lst1,rest)
                else 
                  false 
            | NONE => false 
      in
        if length(lst1) >= length(lst2) then
          depthRecord(lst1,lst2)
        else 
          false 
      end
    | subty (_,_) = false


    
  fun commonSupertype (typ1,typ2) = if (subty(typ1,typ2)) then 
        SOME typ2 
      else 
        if (subty(typ2,typ1)) then 
          SOME typ1
        else 
          NONE


  fun envTypeCheck (env, L.Int n) = T.Int
    | envTypeCheck (env, L.True) = T.Bool
    | envTypeCheck (env, L.False) = T.Bool 
    | envTypeCheck (env, L.Unit) = T.Unit
    | envTypeCheck (env, L.Var str) = (case E.lookup(env,str) of 
          (SOME typ1) => typ1
        | NONE => raise Fail "type not in gamma")
    | envTypeCheck (env, L.Lam(str,typ,t1)) = 
      let 
        val t1type = envTypeCheck(TypeEnv.extend(env,str,typ),t1)
      in 
        T.Function (typ,t1type)
      end
    | envTypeCheck (env, L.App(t1,t2)) =     
      (case (envTypeCheck(env,t1),envTypeCheck(env,t2)) of 
          ((T.Function (typ1,typ2)), typ3) => if subty(typ3,typ1) then 
              typ2
            else
              raise Fail "tau3 is not a subtype of tau1"
        | (_,_) => raise Fail "t1 does not have the right types")
    | envTypeCheck(env, L.Fix(t1)) = (case envTypeCheck (env,t1) of 
          T.Function(tau1,tau1') => tau1
        | _ => raise Fail "fix does not have recursive form")
    | envTypeCheck(env, L.Let(str,t1,t2)) = let
        val newEnv = TypeEnv.extend(env,str,envTypeCheck(env,t1))
      in
        envTypeCheck(newEnv,t2)
      end
    | envTypeCheck(env,L.Cond(c,t1,t2)) = (case envTypeCheck (env,c) of 
          T.Bool => let
              val t1type = envTypeCheck(env,t1)
              val t2type = envTypeCheck(env,t2)
              val t4type = commonSupertype(t1type,t2type)
            in
              case t4type of 
                SOME t4t => t4t
              |NONE => raise Fail "not a supertype"
            end
        | _ => raise Fail "cond t1 is not conditional")
    | envTypeCheck(env, L.Add(t1,t2)) = (case (envTypeCheck(env,t1),envTypeCheck(env,t2)) of 
          (T.Int, T.Int) => T.Int
        | _ => raise Fail "Add does not have interger for both terms")
    | envTypeCheck(env, L.Sub(t1,t2)) = (case (envTypeCheck(env,t1),envTypeCheck(env,t2)) of 
          (T.Int, T.Int) => T.Int
        | _ => raise Fail "Add does not have interger for both terms")
    | envTypeCheck(env, L.Mul(t1,t2)) = (case (envTypeCheck(env,t1),envTypeCheck(env,t2)) of 
          (T.Int, T.Int) => T.Int
        | _ => raise Fail "Add does not have interger for both terms")
    | envTypeCheck(env,L.Eq(t1,t2)) = (case (envTypeCheck(env,t1),envTypeCheck(env,t2)) of 
          (T.Int, T.Int) => T.Bool
        | _ => raise Fail "eq does not have interger for both terms")
    | envTypeCheck(env,L.LessThan(t1,t2)) = (case (envTypeCheck(env,t1),envTypeCheck(env,t2)) of 
          (T.Int, T.Int) => T.Bool
        | _ => raise Fail "Lessthan does not have interger for both terms")
    | envTypeCheck(env,L.Not(t1)) = (case (envTypeCheck (env,t1)) of 
          T.Bool => T.Bool
        | _ => raise Fail "not right Not")
    | envTypeCheck(env,L.Record lst) = 
      let
        fun check (str,t) = (str,envTypeCheck(env,t))
      in
        T.Record(List.map check lst)
      end
    | envTypeCheck(env,L.Select(str,t1)) = case envTypeCheck(env,t1) of 
        T.Record(lst) => (case (List.find (fn (x,y) => x = str) lst) of 
              SOME (_,term) => term
            | NONE => raise Fail "not in record")
      |_ =>raise Fail "select not followed by record"


  fun typeof t = envTypeCheck (TypeEnv.empty,t)
	    
end
