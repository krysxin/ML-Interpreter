open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = tysc Environment.t 
(* type tyenv = ty Environment.t *)
type subst = (tyvar * ty) list 

let rec freevar_tyenv tyenv = (* 型環境に自由に出現している型変数の集合->各型スキームの自由型変数を1つのリスト統合*)
  Environment.fold_right (fun tysc set -> MySet.union set (freevar_tysc tysc)) tyenv MySet.empty


(*型代入を型の等式集合に変換*) 
let eqs_of_subst s = List.map (fun (tv,ty) -> (TyVar tv, ty)) s

  
let rec subst_type subst typ =   
      match subst with
         [] -> typ
       | (tv,ty) :: rest ->
             match typ with 
           | TyInt -> TyInt
           | TyBool -> TyBool
           | TyVar tv' -> if tv' = tv then subst_type rest ty 
                          else subst_type rest typ
           | TyFun (ty1, ty2) -> TyFun (subst_type subst ty1, subst_type subst ty2)
           | _ -> err ("Not Implemented!")

let closure ty tyenv subst = 
    let fv_tyenv' = freevar_tyenv tyenv in
    let fv_tyenv =
      MySet.bigunion
        (MySet.map
            (fun id -> freevar_ty (subst_type subst (TyVar id)))
            fv_tyenv') in
    let ids = MySet.diff (freevar_ty ty) fv_tyenv in
      TyScheme (MySet.to_list ids, ty)


(*型の等式集合に型代入を適用する*)
let rec subst_eqs s eqs = List.map (fun (ty1, ty2) -> (subst_type s ty1, subst_type s ty2)) eqs     

let rec occur_check tv = function 
| TyVar tv' -> tv = tv'
| TyFun (ty1, ty2) -> (occur_check tv ty1) || (occur_check tv ty2)
(* | TyList ty -> Todo*)
| _ -> false
             
let rec unify = function (*等式制約の単一化*)
    [] -> []
  | (ty1, ty2) :: rest -> (match ty1, ty2 with
      TyInt, TyInt | TyBool, TyBool -> unify rest
    | TyFun (ty11, ty12), TyFun (ty21, ty22) -> unify ((ty11, ty21) :: (ty12, ty22) :: rest)
    | TyVar tv1, TyVar tv2 ->
          if tv1 = tv2 then unify rest
          else let s = [(tv1, ty2)] in s @ (unify (subst_eqs s rest))
    | TyVar tv, ty | ty, TyVar tv ->
          if occur_check tv ty then err ("Type Error: Type " ^ string_of_ty ty1 ^ " occured in　" ^ string_of_ty ty2 ^ "!")
          else let s = [(tv, ty)] in s @ (unify (subst_eqs s rest))
    | _, _ -> err ("Unification failed because of type error!"))


   

(*リストの操作関数*)
let get_left (id,_) = id
let get_right (_,e) = e
let get_two l = List.map (fun (id,(_,ty)) -> (id,ty)) l
let get_s l = List.map ( fun (_,(s,_)) -> s) l
let rec append l1 l2 = match l1 with
  [] -> l2
  | x::rest -> x :: append rest l2;;

let ty_prim op ty1 ty2 = match op with 
  Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)



let rec ty_exp (tyenv : tyenv) = function
   Var x ->
      (try 
        let TyScheme (vars, ty) = Environment.lookup x tyenv in
        let s = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
          ([], subst_type s ty)
       with Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([],TyInt)
  | BLit _ -> ([],TyBool)
  | BinOp (op, exp1, exp2) -> (try
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in (* s1 と s2 を等式制約の集合に変換して，eqs3 と合わせる *)
    let s3 = unify eqs in (s3, subst_type s3 ty) with
        _ ->  let op_str = id_of_binop (var_of_binop op) in 
              if op = Plus || op = Mult || op = Lt then 
              err ("Both arguments must be integer: " ^ op_str ) 
              else  err ("Both arguments must be boolean: " ^ op_str ) )  
  | IfExp (exp1, exp2, exp3) -> (try
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (s3, ty3) = ty_exp tyenv exp3 in
    (* if ty1 != TyBool then err ("Type of if expression must be boolean: if") *)
    let eqs =  (eqs_of_subst s1) @ (eqs_of_subst s2) @
          (eqs_of_subst s3) @ [(ty1, TyBool)] @ [(ty2, ty3)] in
          let s4 = unify eqs in (s4, subst_type s4 ty2) with
        _ -> err ("Type Error in if expression: if") )
  | LetExp (e_ls, restexp) -> 
    let id_s_ty = List.map (fun (id,e) -> (id, ty_exp tyenv e)) e_ls in
    let e_s = get_s id_s_ty in (*各idの型代入*)
    let e_eqs = List.map eqs_of_subst e_s in (*各idの型代入の等式制約 *)
    let e_eqs_app = List.fold_left append [] e_eqs in (*各等式制約listを一つのlistに統合*)
    let newtyenv = List.fold_left (*型環境更新*)
          ( fun tyenv' (id', (s', ty')) -> Environment.extend id' (closure ty' tyenv' s') tyenv') tyenv id_s_ty in 
    let (rest_s, rest_ty) = ty_exp newtyenv restexp in
    let eqs = e_eqs_app @ eqs_of_subst rest_s in
    let s = unify eqs in (s,subst_type s rest_ty)
  | LetRecExp (id, para, exp1, exp2) -> 
    let ty_para = TyVar (fresh_tyvar ()) in
    let ty_exp1 = TyVar (fresh_tyvar ()) in
    let ty_id = TyFun (ty_para, ty_exp1) in
    let newtyenv = Environment.extend id (tysc_of_ty ty_id) tyenv in
    let newtyenv2 = Environment.extend para (tysc_of_ty ty_para) newtyenv in
    let (s_e1, ty_e1) = ty_exp newtyenv2 exp1 in
    let s1 = unify (eqs_of_subst s_e1 @ [(ty_e1, ty_exp1)]) in
    let newty = subst_type s1 ty_id in
    let newtyenv3 = Environment.extend id (closure newty tyenv s_e1) tyenv in
    let (s_e2, ty_e2) = ty_exp newtyenv3 exp2 in
    let eqs = (eqs_of_subst s_e2) @ (eqs_of_subst s_e1) @ [(ty_e1,ty_exp1)] in
    let s = unify eqs in (s, subst_type s ty_e2)
  | FunExp (id, exp) ->
    let domty = TyVar (fresh_tyvar ()) in
    let s, ranty = 
      ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp in
        (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let domty = TyVar (fresh_tyvar ()) in
    let eqs = (ty1, TyFun(ty2, domty)) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in 
    let s3 = unify eqs in (s3, subst_type s3 domty) 
  | _ -> err ("Not Implemented!")
  

let ty_decl tyenv = function (*(id,型)と型環境を返す*)
    Exp e -> (["-", get_right (ty_exp tyenv e)], tyenv)
  | Decl e_ls ->
    let id_s_ty = List.map (fun (id,e) -> (id, ty_exp tyenv e)) e_ls in
    let id_ty = List.map (fun (id,e) -> (id, get_right (ty_exp tyenv e))) e_ls in
    let newtyenv = List.fold_left 
        (fun tyenv' (id, (s,ty)) -> Environment.extend id (closure ty tyenv' s) tyenv') tyenv id_s_ty in
         (id_ty, newtyenv)
  | RecDecl (id, para, e) -> 
      let (s, ty) = ty_exp tyenv (LetRecExp (id,para,e,Var id)) in
      let newtyenv = Environment.extend id (closure ty tyenv s) tyenv in (["-", ty], newtyenv)
  | QuitDecl -> exit 0 
  (* | _ -> err ("Not Implemented!") *)
  

