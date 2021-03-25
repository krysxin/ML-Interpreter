open Testutil
open Miniml
open Miniml.Syntax
open Miniml.Typing

type string_of_ty_test_result =
    TyStrOk
  | NotAType of string
  | NotEqual of string

type ('a, 'b) unify_test_result =
    UnifyOk of ('a * 'b) list
  | UnifyErr

type string_of_ty_testcase = { input: string }

type freevar_ty_testcase = { input: string; expected: tyvar MySet.t }

type subst_type_testcase = { subst: (string * string) list; input: string; expected: string }

type unify_testcase = { input: (string * string) list; expected: (string, string) unify_test_result }

let normal_list_of_myset myset =
  List.sort_uniq compare (MySet.to_list myset)

let string_of_myset myset =
  Printf.sprintf
    "{%s}"
    (String.concat ", "
       (List.map string_of_int
          (normal_list_of_myset myset)))

let cmp_for_myset myset1 myset2 =
  normal_list_of_myset myset1 = normal_list_of_myset myset2

let gen_string_of_ty_tests (dataset: string_of_ty_testcase list) =
  let errorhow input actual _ =
    match actual with
      NotAType tystr ->
      Printf.sprintf "reason:    not a type\nactual:    %s\nexpected:  %s" tystr input
    | NotEqual tystr ->
      Printf.sprintf "resaon:    not equal\nactual:    %s\nexpected:  %s" tystr input
    | _ -> "OK"
  in
  let exec input =
    let _, input_ty = TyHelper.ty_of_string input in
    let actual_tystr = string_of_ty input_ty in
    let actual_ty_opt =
      try
        let _, ty = TyHelper.ty_of_string actual_tystr in
        Some ty
      with
        _ -> None
    in
    match actual_ty_opt with
      Some actual_ty ->
      if TyHelper.cmp_ty_alpha input_ty actual_ty then
        TyStrOk
      else
        NotEqual actual_tystr
    | None -> NotAType actual_tystr
  in
  gen_tests2
    ~errorhow: errorhow
    ~cmp: (=)
    ~exec: exec
    (List.map
       (fun (testcase: string_of_ty_testcase) ->
          Testutil.{ input = testcase.input; expected = TyStrOk })
       dataset)

let gen_freevar_ty_tests (dataset: freevar_ty_testcase list) =
  let exec input =
    let _, ty = TyHelper.ty_of_string input in
    let fv = freevar_ty ty in
    fv
  in
  gen_tests
    ~ishow: (fun x -> x)
    ~oshow: string_of_myset
    ~cmp: cmp_for_myset
    ~exec: exec
    (List.map
       (fun (testcase: freevar_ty_testcase) ->
          Testutil.{ input = testcase.input; expected = testcase.expected })
       dataset)

let subst_of_subststr env subst =
  let env, subst = List.fold_left
      (fun (env, subst) (tyvarstr, tystr) ->
         let env, tyvarty = TyHelper.ty_of_string ~tynameenv:env tyvarstr in
         let tyvar = match tyvarty with
           | TyVar tyvar -> tyvar
           | _ -> assert false
         in
         let env, ty = TyHelper.ty_of_string ~tynameenv:env tystr in
         env, (tyvar, ty) :: subst
      )
      (env, [])
      subst
  in
  let subst = List.rev subst in
  env, subst

let eq_of_eqstr env eq =
  let env, eq =
    (List.fold_left
       (fun (env, eq) (tystr1, tystr2) ->
          let env, ty1 = TyHelper.ty_of_string ~tynameenv:env tystr1 in
          let env, ty2 = TyHelper.ty_of_string ~tynameenv:env tystr2 in
          env, (ty1, ty2) :: eq
       )
       (env, [])
       eq)
  in
  let eq = List.rev eq in
  env, eq

let gen_subst_type_tests (dataset: subst_type_testcase list) =
  let exec (subst, ty) = subst_type subst ty in
  gen_tests
    ~ishow: (fun (subst, ty) ->
        Printf.sprintf "subst_type %s %s"
          (TyHelper.rawstring_of_subst subst)
          (TyHelper.rawstring_of_ty ~need_bracket:true ty)
      )
    ~oshow: TyHelper.rawstring_of_ty
    ~cmp: (=)
    ~exec: exec
    (List.map
       (fun (testcase: subst_type_testcase) ->
          let env, input_ty = TyHelper.ty_of_string testcase.input in
          let env, subst = subst_of_subststr env testcase.subst in
          let _, expected = TyHelper.ty_of_string ~tynameenv:env testcase.expected in
          Testutil.{ input = (subst, input_ty); expected = expected })
       dataset)

let gen_unify_tests (dataset: unify_testcase list) =
  let exec eq = try UnifyOk (unify eq) with _ -> UnifyErr in
  gen_tests
    ~ishow: (fun eq ->
        Printf.sprintf "unify %s"
          (TyHelper.rawstring_of_eq eq)
      )
    ~oshow: (function UnifyOk subst -> TyHelper.rawstring_of_subst subst | UnifyErr -> "error")
    ~cmp: (=)
    ~exec: exec
    (List.map
       (fun (testcase: unify_testcase) ->
          let env, input = eq_of_eqstr [] testcase.input in
          let expected =
            match testcase.expected with
            | UnifyOk eqstr ->
              let _, expected = subst_of_subststr env eqstr in
              UnifyOk expected
            | UnifyErr -> UnifyErr
          in
          Testutil.{ input = input; expected = expected })
       dataset)
