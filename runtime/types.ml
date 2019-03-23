type mismatch = {
    ident : string;
    expected : Parsetree.pattern;
    got : Parsetree.pattern;
  }

type failure = {
    common : Parsetree.pattern;
    mismatches : mismatch list;
  }

type ('a, 'b) pattern = 'a -> ('b, failure) result
