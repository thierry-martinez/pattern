include module type of struct
  include Types
end

val format_failure : Format.formatter -> failure -> unit

val elim_type_constraints : Ppxlib.expression -> Ppxlib.expression
