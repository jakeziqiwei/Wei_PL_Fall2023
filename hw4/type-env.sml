structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
  val printEnv : env -> unit 

end = struct

  type env = (string * Type.typ) list
	       
  val empty = [] 

  fun lookup (env,var) = case (List.find (fn (x,_) : (string * Type.typ)  => x = var) env) of 
        NONE => NONE 
      | SOME (_,typ) => SOME(typ)

    
  fun extend (env,var,typ) = let
        val lookUp = lookup(env,var)
      in
        case lookUp of 
          NONE => (var,typ) :: env
        | SOME t => (var,typ) :: (List.filter (fn (x,_) : (string * Type.typ) => x <> var) env)
      end

  fun printEnv(env) =
      let
        fun help [] = (print ("Not Env Left");())
          | help ((v,t)::xs) = (
              print (v ^ ": " ^ (Type.tos t) ^ "\n");
              help xs
            )
      in
        print "Environment:\n";
        help env;
        print "end\n"
      end



end
