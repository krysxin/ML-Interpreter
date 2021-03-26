open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref  
  | DProcV of id * exp  
  (* | ConsV of exval * exval 
  | NilV *)
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (_,_,_) -> "<fun>" 
  | DProcV(_,_) -> "<dfun>" 

let pp_val v = print_string (string_of_exval v)

(*bound errorを表示させる*)
let boundError () = err ("Same variable is bound several times")
let get_id l = List.map (fun (id, _) -> id ) l
(*同じ変数が複数回束縛されたかどうかを判断する*)
let isBoundSeveralTimes l =
  let idlist = get_id l in
  let rec bound idlist =
  match idlist with
  [] -> false
  | x :: [] -> false
  | x :: y :: rest -> 
    if x=y then true 
    else bound (x :: rest) || bound (y :: rest) in bound idlist

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Or, BoolV i1, BoolV i2 -> BoolV (i1 || i2)  
  | Or, _, _ -> err ("Both arguments must be boolean: ||")
  | And, BoolV i1, BoolV i2 -> BoolV (i1 && i2) 
  | And, _, _ -> err ("Both arguments must be boolean: &&")


let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    if (op = And && arg1 = BoolV false) then BoolV false  
    else if (op = Or && arg1 = BoolV true) then BoolV true 
    else let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (ls,restexp) ->
    if isBoundSeveralTimes ls then boundError();
    let id_vals = List.map (fun (id, e) -> (id, eval_exp env e)) ls in
     let newenv = List.fold_left (fun e (id, v) -> Environment.extend id v e) env id_vals in
     eval_exp newenv restexp

  | FunExp (id, exp) -> ProcV (id, exp, ref env) 
  | DFunExp (id,exp) -> DProcV (id,exp)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
          ProcV (id, body, env') -> 
              let newenv = Environment.extend id arg env'.contents in
                eval_exp newenv body
        | DProcV (id, body) -> let newenv = Environment.extend id arg env in 
                eval_exp newenv body
        | _ -> err ("Non-function value is applied")) (* 上記のいずれでもなければ，実行時型エラー *)
  
  | LetRecExp (id, para, exp1, exp2) ->
    let dummyenv = ref Environment.empty in (* ダミーの環境への参照を作る *)
    (* 関数閉包を作り，idをこの関数閉包に写像するように現在の環境envを拡張 *)
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
    (* ダミーの環境への参照に，拡張された環境を破壊的代入してバックパッチ *)
        dummyenv := newenv;
        eval_exp newenv exp2
  

let eval_decl env = function 
    Exp e -> let v = eval_exp env e in (["-", v], env)
  (* | Decl (id, e) ->
    let v = eval_exp env e in (id, Environment.extend id v env, v) *)
  | Decl e_ls -> 
       if isBoundSeveralTimes e_ls then boundError();
       let v_ls = List.map (fun (id, e) -> (id, eval_exp env e)) e_ls in
       let newenv = List.fold_left (fun e (id, v) -> Environment.extend id v e) env v_ls in
         (v_ls, newenv)
  | RecDecl (id, para, e) ->
    let dummyenv = ref Environment.empty in
    let v = (ProcV (para, e, dummyenv)) in
    let newenv = Environment.extend id v env in 
    dummyenv := newenv;
    (["-", v], newenv)
  | QuitDecl -> exit 0 (*quit interactive session*)
  


  
  
