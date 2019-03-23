include Types

let format_failure formatter failure =
  Format.fprintf formatter "@[<v>@[%a@]" Pprintast.pattern failure.common;
  failure.mismatches |> List.iter begin fun mismatch ->
    Format.fprintf formatter
      "@[%s:@ @[<v>@[@[Expected:@]@ @[%a@]@]@,@[@[Got:@]@ @[%a@]@]@]@]"
      mismatch.ident Pprintast.pattern mismatch.expected
      Pprintast.pattern mismatch.got
  end;
  Format.fprintf formatter "@]"
