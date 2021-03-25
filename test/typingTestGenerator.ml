open Testutil
open Miniml
(* open Miniml.Cui *)
open Miniml.Typing
open Miniml.Syntax

type typedcase = { input: string; expected: string }
type errorcase = { input: string }

type typing_to_raise_result =
  | Typed of ty
  | ErrorRaised

let cmp_ty_alpha (ty1: ty) (ty2: ty) =
  (* make a map from tyvars in ty1 to tyvars in ty2 *)
  let rec unify eq ty1 ty2 =
    match ty1, ty2 with
      TyInt, TyInt | TyBool, TyBool -> Some eq
    | TyVar id1, TyVar id2 ->
      (try
         if List.assoc id1 eq = id2 then
           Some eq
         else
           None
       with
         Not_found -> Some ((id1, id2) :: eq)
       | _ -> Some eq
      )
    | TyFun (tyarg1, tybody1), TyFun (tyarg2, tybody2) ->
      (match unify eq tyarg1 tyarg2 with
         Some eq ->
         unify eq tybody1 tybody2
       | None -> None
      )
    | TyList ty1, TyList ty2 ->
      unify eq ty1 ty2
    | _ -> None
  in
  let check_eqrev eq =
    let values = List.map (fun (_, b) -> b) eq in
    List.length eq = List.length (List.sort_uniq compare values)
  in
  match unify [] ty1 ty2 with
    Some eq ->
    (* reject [(1, 2); (2, 2)] and so on *)
    check_eqrev eq
  | None -> false

let convert_ty ty =
  let rec convert env = function
      TySyntax.TyInt -> env, TyInt
    | TySyntax.TyBool -> env, TyBool
    | TySyntax.TyVar name ->
      (match List.assoc_opt name env with
         Some id ->
         env, TyVar id
       | None ->
         let id = List.length env in
         let env = (name, id) :: env in
         env, TyVar id)
    | TySyntax.TyFun (ty1, ty2) ->
      let env, ty1 = convert env ty1 in
      let env, ty2 = convert env ty2 in
      env, TyFun (ty1, ty2)
    | TySyntax.TyList ty ->
      let env, ty = convert env ty in
      env, TyList ty
  in
  let _, ty = convert [] ty in
  ty

(* original *)
(* let typing input =
  Exec.exec
    (fun env program ->
       (* let env, ty = ty_decl env program *)
       let ty = ty_decl env program in
       env, ty)
    Cui.initial_tyenv
    input *)

(*new try*)
let typing input =
  Exec.exec
    (fun env program ->
        (* let env, ty = ty_decl env program *)
        let (ty_ls, new_env) = ty_decl env program in
        let (_,ty) =  List.hd ty_ls in
        new_env, ty)
    Cui.initial_tyenv
    input


let typing_to_raise src =
  try
    let ty = typing src in
    Typed ty
  with
    Exec.Error msg -> raise (Exec.Error msg)
  | _ -> ErrorRaised

let typing_to_raise_result_printer = function
  | Typed ty -> string_of_ty ty
  | ErrorRaised -> "error"

let gen_typing_tests (dataset: typedcase list) =
  gen_tests
    ~ishow: (fun x -> x)
    ~oshow: string_of_ty
    ~cmp: TyHelper.cmp_ty_alpha
    ~exec: typing
  @@ List.map
    (fun (testcase: typedcase): (string, ty) test ->
       let _, expected = TyHelper.ty_of_string testcase.expected in
       { input = testcase.input; expected = expected })
    dataset

let gen_typingerror_tests (dataset: errorcase list) =
  gen_tests
    ~ishow: (fun x -> x)
    ~oshow: typing_to_raise_result_printer
    ~cmp: (=)
    ~exec: typing_to_raise
  @@ List.map
    (fun (testcase: errorcase): (string, typing_to_raise_result) test ->
       { input = testcase.input; expected = ErrorRaised })
    dataset
