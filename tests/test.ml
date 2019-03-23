let () =
  assert ([%pattern? 10] 10 = Ok ());
  assert ([%pattern? _] 10 = Ok ());
  assert (
    match [%pattern? 42] 10 with
    | Error {
       common = { ppat_desc = Ppat_var { txt = "@0"; _ }; _};
       mismatches = [{
        ident = "@0";
        expected = { ppat_desc = Ppat_constant (Pconst_integer ("42", None)); _};
        got = { ppat_desc = Ppat_constant (Pconst_integer ("10", None)); _}}]} -> true
    | _ -> false);
  assert (
    match [%pattern? (18 | 42)] 20 with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true)
