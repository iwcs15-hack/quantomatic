structure EE_Data =
struct
  val pretty_theory_name = Pretty.str "compact_closed"
  type psubst = unit
  type subst  = unit

  datatype nvdata = EEnd (* Epsilon/Eta node *) 
                  | Var of string
  val default_nvdata = Var ""

  fun init_psubst_from_data _ _ = ()

  fun default_nvdata_of_typestring s =
    case s of "EEnd" => EEnd
            | "var" => Var ""
            | _ => raise unknown_typestring_exp s

  fun nvdata_eq (EEnd, EEnd) = true
    | nvdata_eq (Var s, Var t) = (s = t)
    | nvdata_eq _ = false
      
  fun pretty_nvdata EEnd = 
      Pretty.str "EE"
    | pretty_nvdata (Var s) = Pretty.block [Pretty.str "Var(", Pretty.str s, Pretty.str ")"]
  
  fun match_nvdata (EEnd, EEnd) m = SOME m
    | match_nvdata (Var s, Var t) m = if s = t then SOME m else NONE
    | match_nvdata _ _ = NONE

  fun subst_in_nvdata sub EEnd = (sub, EEnd)
    | subst_in_nvdata sub (Var s) = (sub, Var s)

  open EmptyEdgeData

  val solve_psubst = Seq.single
end
