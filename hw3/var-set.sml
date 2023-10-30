structure VarSet :> sig

  type set

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set
  val print_set : set -> unit

end = struct

  type set = string list (* <== Change this to something else! *)

  val empty = []  (* <== Change this to something consistent with the new set type. *)

  fun mem (str,set) = (List.exists (fn x:string => x = str) set) 

  fun ins (str,set) = str :: set 

  fun rem (str,set) = (List.filter (fn x:string=> x <> str) set)

  fun union ([],set2) = set2 
    | union ((x::xs), set2) = if (List.exists (fn s:string => s = x) set2) then 
        union (xs,set2)
      else 
        x::union(xs,set2)

  fun print_set(set) =
      let
        fun help [] = ()
          | help (x::xs) = (
              print (x ^ " ");
              help xs
            )
      in
        print "[ ";
        help set;
        print "]\n"
      end




				      
end
