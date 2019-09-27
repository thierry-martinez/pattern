include module type of struct
  include Types
end

val check :
    ('a -> Parsetree.expression) -> 'a -> ('a, 'b) matcher ->
      'b pattern_result

val format_failure : Format.formatter -> failure -> unit

val elim_type_constraints : Parsetree.expression -> Parsetree.expression
