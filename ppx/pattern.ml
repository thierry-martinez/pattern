let make_pat_constant_integer ~loc s suffix : Parsetree.expression =
  match suffix with
  | None ->
      [%expr Ast_helper.Pat.constant (Ast_helper.Const.integer [%e s])]
  | Some suffix ->
      let suffix = Ast_helper.Exp.constant (Ast_helper.Const.char suffix) in
      [%expr Ast_helper.Pat.constant (Ast_helper.Const.integer ~suffix:[%e suffix] [%e s])]

let rec expr_of_pat (pat : Parsetree.pattern) : Parsetree.expression =
  let loc = pat.ppat_loc in
  match pat.ppat_desc with
  | Ppat_any -> [%expr Ppat_any]
  | Ppat_constant constant ->
      begin
        match constant with
        | Pconst_integer (s, suffix) ->
            make_pat_constant_integer ~loc
              (Ast_helper.Exp.constant (Ast_helper.Const.string s)) suffix
        | Pconst_char c ->
            [%expr Ast_helper.Pat.constant (Ast_helper.Const.char [%e Ast_helper.Exp.constant (Ast_helper.Const.char c)])]
        | Pconst_string (s, _) ->
            [%expr Ast_helper.Pat.constant (Ast_helper.Const.string [%e Ast_helper.Exp.constant (Ast_helper.Const.string s)])]
        | Pconst_float (f, _) ->
            [%expr Ast_helper.Pat.constant (Ast_helper.Const.float [%e Ast_helper.Exp.constant (Ast_helper.Const.float f)])]
      end
  | Ppat_or (a, b) ->
      let a = expr_of_pat a in
      let b = expr_of_pat b in
      [%expr Ast_helper.Pat.or_ [%e a] [%e b]]
  | _ ->
      raise (Location.Error (Location.error ~loc "unimplemented"))

let rec make_matcher_res binder (pat : Parsetree.pattern) : (Parsetree.expression * 'a) =
  let loc = pat.ppat_loc in
  let exp_binder = Ast_helper.Exp.ident ~loc binder in
  let expected = expr_of_pat pat in
  match pat.ppat_desc with
  | Ppat_any ->
      [%expr Ok ()], None
  | Ppat_constant constant ->
      let got : Parsetree.expression =
        match constant with
        | Pconst_integer (_, suffix) ->
            let got : Parsetree.expression =
              match suffix with
              | None ->
                  [%expr string_of_int [%e exp_binder]]
              | Some 'l' ->
                  [%expr Int32.to_string [%e exp_binder]]
              | Some 'L' ->
                  [%expr Int64.to_string [%e exp_binder]]
              | Some 'n' ->
                  [%expr Nativeint.to_string [%e exp_binder]]
              | Some _ ->
                  raise (Location.Error (Location.error ~loc "unknown integer type")) in
            make_pat_constant_integer ~loc got suffix
        | Pconst_char _ ->
            [%expr Ast_helper.Pat.constant (Ast_helper.Const.char [%e exp_binder])]
        | Pconst_string (_, _) ->
            [%expr Ast_helper.Pat.constant (Ast_helper.Const.string [%e exp_binder])]
        | Pconst_float (_, _) ->
            [%expr Ast_helper.Pat.constant (Ast_helper.Const.float [%e exp_binder])] in
      [%expr
        if [%e exp_binder] = [%e Ast_helper.Exp.constant constant] then
          Ok ()
        else
          let index = !_diff_counter in
          _diff_counter := succ index;
          let ident = Printf.sprintf "@%d" index in
          Error { Pattern_runtime.common = Ast_helper.Pat.var { txt = ident; loc = Location.none }; mismatches = [{ Pattern_runtime.ident = ident; expected = [%e expected]; got = [%e got]}]}], Some got
  | Ppat_or (a, b) ->
      let a, got = make_matcher_res binder a in
      let b, _ = make_matcher_res binder b in
      begin
        match got with
        | None ->
            [%expr
              match [%e a] with
              | Ok result -> Ok result
              | Error _ -> assert false]
        | Some got ->
            [%expr
              let index = !_diff_counter in
              match [%e a] with
              | Ok result -> Ok result
              | Error _ ->
                  match [%e b] with
                  | Ok result -> Ok result
                  | Error _ ->
                      _diff_counter := succ index;
                      let ident = Printf.sprintf "@%d" index in
                      Error { Pattern_runtime.common = Ast_helper.Pat.var { txt = ident; loc = Location.none }; mismatches = [{ Pattern_runtime.ident = ident; expected = [%e expected]; got = [%e got]}]}]
      end, got
  | _ ->
      raise (Location.Error (Location.error ~loc "unimplemented"))

let make_matcher (pat : Parsetree.pattern) : Parsetree.expression =
  let loc = pat.ppat_loc in
  let result, _got = make_matcher_res { loc; txt = Longident.Lident "_x" } pat in
  [%expr fun _x -> let _diff_counter = ref 0 in [%e result]]

let expr_mapper (mapper : Ast_mapper.mapper) (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_extension ({ loc; txt = "pattern" }, pat) ->
      begin
        match pat with
        | PPat (pat, None) ->
            make_matcher pat
        | PPat (_pat, Some _) ->
            raise (Location.Error (Location.error ~loc
              "unexcepted when clause"))
        | _ ->
            raise (Location.Error (Location.error ~loc
              "pattern \"? ...\" expected"))
      end
  | _ ->
      Ast_mapper.default_mapper.expr mapper expr

let ppx_pattern_mapper = {
  Ast_mapper.default_mapper with
  expr = expr_mapper
}

let () =
  Migrate_parsetree.Driver.register ~name:"ppx_pattern"
    (module Migrate_parsetree.OCaml_current)
    (fun _ _ -> ppx_pattern_mapper)
