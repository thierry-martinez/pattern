let build_pat ~loc pat : Parsetree.expression =
  [%expr (
   { ppat_desc = [%e pat]; ppat_loc = Location.none; ppat_attributes = [] }
     : Parsetree.pattern)]

let build_pat_construct ~loc ctor arg : Parsetree.expression =
  let lift_loc = Ppxlib_metaquot.Expr.lift loc in
  build_pat ~loc [%expr Ppat_construct (
    [%e lift_loc#loc lift_loc#longident ctor], [%e arg])]

let sub i = Printf.sprintf "sub%d" i

let quoted i = Printf.sprintf "quoted%d" i

let pat_var_of_string ~loc s =
  Ast_helper.Pat.var { loc; txt = s }

let exp_var_of_string ~loc s =
  Ast_helper.Exp.ident { loc; txt = Lident s }

let pat_tuple_or_value ~loc list =
  match list with
  | [value] -> value
  | _ -> Ast_helper.Pat.tuple ~loc list

let exp_tuple_or_value ~loc list =
  match list with
  | [value] -> value
  | _ -> Ast_helper.Exp.tuple ~loc list

let rec make_matcher (pat : Parsetree.pattern) : Parsetree.expression =
  let loc = pat.ppat_loc in
  let mismatch () : Parsetree.expression =
    [%expr
       __mismatch_count_ref := index + 1;
     let ident = Printf.sprintf "@%d" index in
     Error {
     common = {
       ppat_desc = Ppat_var { loc = Location.none; txt = ident };
       ppat_loc = Location.none; ppat_attributes = [] };
     mismatches = [{
       ident;
       expected = Parsetree.(
         let loc = Location.none in
         [%e (Ppxlib_metaquot.Expr.lift Location.none)#pattern pat]);
       got = quoted; }]}] in
  let multiple_match patterns get_pattern destruct
    (destruct_quoted :
       Parsetree.expression -> string list -> Parsetree.expression ->
         Parsetree.expression -> Parsetree.expression) build_common =
    let subs = List.mapi (fun i _ -> sub i) patterns in
    let sub_pats = List.map (pat_var_of_string ~loc) subs in
    let quoteds = List.mapi (fun i _ -> quoted i) patterns in
    destruct sub_pats (fun () : Parsetree.expression ->
        [%expr
           let [%p pat_tuple_or_value ~loc (List.map begin
             fun pat : Parsetree.pattern ->
               [%pat? ([%p pat_var_of_string ~loc pat]
                 : Parsetree.expression option)]
           end quoteds)] =
             match quoted with
             | None ->
                 [%e exp_tuple_or_value ~loc (patterns |> List.mapi begin
                   fun i _ : Parsetree.expression ->
                     [%expr None]
                 end)]
             | Some quoted ->
             [%e destruct_quoted
                [%expr Pattern_runtime.elim_type_constraints quoted] quoteds
                (exp_tuple_or_value ~loc (patterns |> List.mapi begin
                  fun i _ : Parsetree.expression ->
                    [%expr Some [%e exp_var_of_string ~loc (quoted i)]]
                end))
                (exp_tuple_or_value ~loc (patterns |> List.mapi begin
                  fun i _ : Parsetree.expression ->
                    [%expr None]
                end))] in
           match [%e exp_tuple_or_value ~loc (patterns |> List.mapi begin
             fun i arg : Parsetree.expression ->
               [%expr
                 let quoted = [%e exp_var_of_string ~loc (quoted i)] in
                 let v = [%e exp_var_of_string ~loc (sub i)] in
                 [%e make_matcher (get_pattern arg)]]
             end)] with
           | [%p pat_tuple_or_value ~loc
                (patterns |> List.mapi begin
                  fun _ _ : Parsetree.pattern -> [%pat? Ok ()]
                end)] -> Ok ()
           | [%p pat_tuple_or_value ~loc sub_pats] ->
               let common = let loc = Location.none in
               [%e build_common (subs |> List.map begin
                 fun sub : Parsetree.expression ->
                   [%expr (match [%e exp_var_of_string ~loc sub] with
                   | Ok () -> [%e build_pat ~loc [%expr Ppat_any]]
                   | Error error -> error.common)]
               end)] in
               let mismatches =
                 List.flatten [%e List.fold_right begin
                   fun sub list : Parsetree.expression ->
                     [%expr (match [%e exp_var_of_string ~loc sub] with
                     | Ok () -> []
                     | Error error -> error.mismatches) :: [%e list]]
                 end subs [%expr []]] in
               Error { common; mismatches }]) in
  let multiple_match_tuple args destruct
      (destruct_quoted : Parsetree.pattern -> Parsetree.pattern) build_common =
    multiple_match args Fun.id
      (fun sub_pats k -> destruct (Ast_helper.Pat.tuple ~loc sub_pats) k)
      (fun quoted quoteds success none ->
        [%expr match [%e quoted] with
        | [%p destruct_quoted [%pat?
            { pexp_desc = Pexp_tuple [%p List.fold_right begin
              fun var list : Parsetree.pattern ->
                [%pat? [%p pat_var_of_string ~loc var] :: [%p list]]
            end quoteds [%pat? []]]; _ }]] -> [%e success]
        | _ -> [%e none]])
      (fun args -> build_common (
        build_pat ~loc [%expr Ppat_tuple
          [%e (Ppxlib_metaquot.Expr.lift loc)#list Fun.id args]])) in
  let multiple_match_record fields closed_flag destruct
      (destruct_quoted : Parsetree.pattern -> Parsetree.pattern) build_common =
    multiple_match
      fields (fun (_label, pat) -> pat)
      (fun sub_pats k ->
        destruct (Ast_helper.Pat.record
          (List.combine (List.map fst fields) sub_pats) closed_flag) k)
      (fun quoted quoteds success none ->
        [%expr
              let extract_field
                  [%p pat_tuple_or_value ~loc
                     (List.map (pat_var_of_string ~loc) quoteds)]
                  ((label : Longident.t Location.loc), value) =
                [%e Ast_helper.Exp.match_ [%expr label.txt]
                   (List.mapi (fun i ((label : Longident.t Location.loc), _) ->
                     Ast_helper.Exp.case
                       ((Ppxlib_metaquot.Patt.lift loc)#longident label.txt)
                       (exp_tuple_or_value ~loc (List.mapi begin
                         fun j q : Parsetree.expression ->
                           if i = j then
                             [%expr Some value]
                           else
                             exp_var_of_string ~loc q
                       end quoteds))) fields @
                    [Ast_helper.Exp.case (Ast_helper.Pat.any ~loc ())
                      (exp_tuple_or_value ~loc
                         (List.map (exp_var_of_string ~loc) quoteds))])] in
              match [%e quoted] with
              | [%p destruct_quoted [%pat?
                    { pexp_desc = Pexp_record (fields, None); _ }]] ->
                  begin
                    match List.fold_left extract_field [%e none] fields with
                    | [%p pat_tuple_or_value ~loc
                         (List.map
                            (fun q : Parsetree.pattern ->
                              [%pat? Some [%p pat_var_of_string ~loc q]])
                            quoteds)] -> [%e success]
                    | _ -> [%e none]
                  end
              | _ -> [%e none]])
          (fun args ->
            let lift = Ppxlib_metaquot.Expr.lift loc in
            build_common (build_pat ~loc
              [%expr Ppat_record
              ([%e lift#list lift#tuple (List.map2 (fun (label, _) value ->
                [lift#loc lift#longident label; value]) fields args)],
                [%e lift#closed_flag closed_flag])])) in
  let rec make_matcher' (pat : Parsetree.pattern)
      (type_constr : Parsetree.pattern -> Parsetree.pattern)
        : Parsetree.expression =
    match pat with
    | [%pat? ([%p? pat] : [%t? ty])] ->
       make_matcher' pat (fun contents -> [%pat? ([%p contents] : [%t ty])])
    | [%pat? _] -> [%expr Ok ()]
    | { ppat_desc = Ppat_constant constant; _ } ->
        [%expr
          match v with
          | [%p type_constr (Ast_helper.Pat.constant ~loc constant)] ->
            Ok ()
          | _ ->
              let index = !__mismatch_count_ref in
              [%e mismatch ()]]
    | [%pat? ([%p? a] | [%p? b])] ->
        [%expr
          let index = !__mismatch_count_ref in
          match [%e make_matcher a] with
          | Ok () -> Ok ()
          | Error _ ->
              match [%e make_matcher b] with
              | Ok () -> Ok ()
              | Error error_b -> [%e mismatch ()]]
    | { ppat_desc = Ppat_construct (ctor, None); _ } ->
        [%expr
           match v with
           | [%p type_constr (Ast_helper.Pat.construct ctor None)] -> Ok ()
           | _ ->
               begin
                 let index = !__mismatch_count_ref in
                 [%e mismatch ()]
               end]
    | { ppat_desc = Ppat_construct (ctor, Some
          { ppat_desc = Ppat_tuple args; _ }); _ } ->
        multiple_match_tuple
          args
          (fun sub_pats k : Parsetree.expression ->
            [%expr
               match v with
               | [%p type_constr
                    (Ast_helper.Pat.construct ctor (Some sub_pats))] ->
                   [%e k ()]
               | _ ->
                   begin
                     let index = !__mismatch_count_ref in
                     [%e mismatch ()]
                   end])
          (fun quoteds ->
            [%pat? { pexp_desc = Pexp_construct (_ctor, Some [%p quoteds]); _}])
          (fun args -> build_pat_construct ~loc ctor [%expr (Some [%e args])])
    | { ppat_desc = Ppat_construct (ctor, Some
          { ppat_desc = Ppat_record (fields, closed_flag); _ }); _ } ->
        multiple_match_record
          fields closed_flag
          (fun sub_pats k : Parsetree.expression ->
            [%expr
               match v with
               | [%p type_constr
                    (Ast_helper.Pat.construct ctor (Some sub_pats))] ->
                   [%e k ()]
               | _ ->
                   begin
                     let index = !__mismatch_count_ref in
                     [%e mismatch ()]
                   end])
          (fun quoteds ->
            [%pat? { pexp_desc = Pexp_construct (_ctor, Some [%p quoteds]); _}])
          (fun args -> build_pat_construct ~loc ctor [%expr (Some [%e args])])
    | { ppat_desc = Ppat_construct (ctor, Some pat); _ } ->
        [%expr
           match v with
           | [%p type_constr
                (Ast_helper.Pat.construct ctor (Some [%pat? sub]))] ->
               begin
                 match
                   let (quoted : Parsetree.expression option) =
                     match quoted with
                     | None -> None
                     | Some quoted ->
                     match Pattern_runtime.elim_type_constraints quoted with
                     | { pexp_desc = Pexp_construct (_ctor, Some arg); _ } ->
                         Some arg
                     | _ -> None in
                   let v = sub in [%e make_matcher pat] with
                 | Ok () -> Ok ()
                 | Error error ->
                     Error {
                       common =
                         (let loc = Location.none in
                         [%e build_pat_construct ~loc ctor
                           [%expr (Some error.common)]]);
                       mismatches = error.mismatches }
               end
           | _ ->
               begin
                 let index = !__mismatch_count_ref in
                 [%e mismatch ()]
               end]
    | { ppat_desc = Ppat_tuple args; _ } ->
        multiple_match_tuple args
          (fun sub_pats k : Parsetree.expression ->
            [%expr match v with [%p type_constr sub_pats] -> [%e k ()]])
          Fun.id Fun.id
    | { ppat_desc = Ppat_record (fields, closed_flag); _ } ->
        multiple_match_record fields closed_flag
          (fun sub_pats k : Parsetree.expression ->
            [%expr match v with [%p type_constr sub_pats] -> [%e k ()]])
          Fun.id Fun.id
    | _ ->
        failwith (Format.asprintf "unimplemented: %a" Pprintast.pattern pat) in
  let matcher = make_matcher' pat Fun.id in
  [%expr ([%e matcher] : _ Pattern_runtime.pattern_result)]

let expr_mapper (mapper : Ast_mapper.mapper) (expr : Parsetree.expression)
    : Parsetree.expression =
  match expr.pexp_desc with
  | Pexp_extension ({ loc; txt = "pattern" }, pat) ->
      begin
        match pat with
        | PPat (pat, None) ->
            [%expr ((fun ?quoted v ->
              let __mismatch_count_ref = ref 0 in
              [%e make_matcher pat])
                : (_, _) Pattern_runtime.matcher) [@ocaml.warning "-27"]]
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
