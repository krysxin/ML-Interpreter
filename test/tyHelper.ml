open Miniml.Syntax

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
    | TyList ty1, TyList ty2 -> unify eq ty1 ty2
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

let rec convert_ty env = function
  | TySyntax.TyInt -> env, TyInt
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
    let env, ty1 = convert_ty env ty1 in
    let env, ty2 = convert_ty env ty2 in
    env, TyFun (ty1, ty2)
  | TySyntax.TyList ty ->
    let env, ty = convert_ty env ty in
    env, TyList ty

let ty_of_string ?(tynameenv=[]) tystr =
  let lexbuf = Lexing.from_string tystr in
  convert_ty tynameenv (TyParser.toplevel TyLexer.main lexbuf)

let rec rawstring_of_ty ?(need_bracket=false) ty =
  let add_bracket tystr =
    if need_bracket then
      Printf.sprintf "(%s)" tystr
    else
      tystr
  in
  match ty with
  | TyBool -> "TyBool"
  | TyInt -> "TyInt"
  | TyVar tyvar ->
    Printf.sprintf "TyVar %d" tyvar
    |> add_bracket
  | TyFun (ty1, ty2) ->
    Printf.sprintf "TyFun (%s) (%s)"
      (rawstring_of_ty ~need_bracket:true ty1)
      (rawstring_of_ty ~need_bracket:true ty2)
    |> add_bracket
  | TyList ty ->
    rawstring_of_ty ~need_bracket:true ty
    |> Printf.sprintf "TyList %s"
    |> add_bracket

let rawstring_of_subst subst =
  List.map
    (fun (tyvar, ty) -> Printf.sprintf "(%d, %s)" tyvar (rawstring_of_ty ty))
    subst
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let rawstring_of_eq eq =
  List.map
    (fun (ty1, ty2) -> Printf.sprintf "%s = %s" (rawstring_of_ty ty1) (rawstring_of_ty ty2))
    eq
  |> String.concat "; "
  |> Printf.sprintf "[%s]"
