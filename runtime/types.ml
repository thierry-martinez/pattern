type pattern =
    ((Parsetree.pattern [@mapopaque]) [@printer Ppxlib.Pprintast.pattern])

and expression =
    ((Parsetree.expression [@mapopaque])
       [@printer Ppxlib.Pprintast.expression])

and mismatch = {
    ident : string;
    expected : pattern;
    got : expression option;
  }

and failure = {
    common : pattern;
    mismatches : mismatch list;
  }
      [@@deriving refl]

type 'a pattern_result = ('a, failure) result
      [@@deriving refl]

type ('a, 'b) matcher = ?quoted:Parsetree.expression -> 'a -> 'b pattern_result
