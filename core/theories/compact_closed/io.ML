(* Data input for red-green graphs. The input strives to be as backward-compatible
 * as possible, while output always outputs the newest format. As a consequence,
 * the old Quantomatic GUI will no longer talk to the core. *)

structure EE_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = EE_Data.nvdata
  type edata = EE_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "ee" => EE_Data.EEnd
              | "var" => EE_Data.Var ""
              | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
      | input (Json.Object obj) =
          (case to_lower (get_string obj "type")
             of "ee" => EE_Data.EEnd
              | "var" => EE_Data.Var (case Json.lookup obj "value"
                                        of SOME (Json.String s) => s
                                         | NONE => "")
              | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected object","")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = nvdata
    fun output EE_Data.EEnd =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "EE")
          )
      | output (EE_Data.Var s) =
          Json.Object (
            Json.empty_obj |> update ("type", Json.String "var")
                           |> update ("value", Json.String s)
          )
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data (EE_Data.Var _) =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
      | style_for_ivertex_data EE_Data.EEnd =
          "[style=filled,fillcolor=yellow,fontcolor=white,shape=square]"
  end
end


