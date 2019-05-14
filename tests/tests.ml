type record = { x : int; y : int; z : int }
      [@@deriving quote]

type labeled_tuple = A of int * int | B of bool * bool * bool
    [@@deriving quote]

type labeled_record = C of { x : int; y : int } | D of { x : bool; y : bool; z : bool }
    [@@deriving quote]

let () =
  assert ([%pattern? 10] 10 = Ok ());
  assert ([%pattern? _] 10 = Ok ());
  assert (
    match [%pattern? 42] ~quoted:([%derive.quote: int] 10) 10 with
    | Error {
       common = { ppat_desc = Ppat_var { txt = "@0"; _ }; _};
       mismatches = [{
        ident = "@0";
        expected = { ppat_desc = Ppat_constant (Pconst_integer ("42", None)); _};
        got = Some { pexp_desc = Pexp_constant (Pconst_integer ("10", None)); _}}]} -> true
    | got ->
        Format.fprintf Format.err_formatter "%a@." (Pattern_runtime.pp_pattern_result (fun _ _ -> ())) got;
        false);
  assert (
    match [%pattern? (18 | 42)] ~quoted:([%derive.quote: int] 20) 20 with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? false] ~quoted:([%derive.quote: bool] true) true with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? None] ~quoted:([%derive.quote: bool option] (Some true)) (Some true) with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? Some false] ~quoted:([%derive.quote: bool option] (Some true)) (Some true) with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? (1, 2, 3)] ~quoted:([%derive.quote: int * int * int] (3, 2, 1)) (3, 2, 1) with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert ([%pattern? { z = 1; x = 2; _ }] { x = 2; y = 3; z = 1 } = Ok ());
  assert (
    match [%pattern? { z = 1; x = 2; _ }] ~quoted:([%derive.quote: record] { x = 1; y = 2; z = 3 }) { x = 1; y = 2; z = 3 } with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? (A (1, 2))] ~quoted:([%derive.quote: labeled_tuple] (A (3, 2))) (A (3, 2)) with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? (B (true, false, true))] ~quoted:([%derive.quote: labeled_tuple] (B (false, true, false))) (B (false, true, false)) with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? (C { y = 1; _ })] ~quoted:([%derive.quote: labeled_record] (C { x = 3; y = 2 })) (C { x = 3; y = 2 }) with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);
  assert (
    match [%pattern? (D { z = false; y = false; _ })] ~quoted:([%derive.quote: labeled_record] (D { x = false; y = true; z = false})) (D { x = false; y = true; z = false }) with
    | Ok _ -> false
    | Error failure ->
        Format.printf "%a@." Pattern_runtime.format_failure failure;
        true);

