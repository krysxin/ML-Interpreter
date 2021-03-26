open Eval
open Syntax 
open Typing 

let rec read_eval_print env tyenv = 
  print_string "# ";
  flush stdout;
  let err_out str = Printf.printf "%s" str;  (*エラーメッセージ出力して、プロンプトに戻る*)
  print_newline();
  read_eval_print env tyenv in
  (try
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (* 型推論　type listを追加 *)
  let (id_ty, newtyenv) = ty_decl tyenv decl in
  let (idv, newenv) = eval_decl env decl in
  (* 型推論　(id,v,ty)を要素とするリストを獲得 *)
  let id_v_ty = List.map2 (fun (id,v) (_,ty) -> (id,v,ty)) idv id_ty in
  List.iter (fun (id, v,ty) ->  
     Printf.printf "val %s : " id; 
     pp_ty ty;
     print_string " = ";
     pp_val v; 
     print_newline()) id_v_ty; 
  read_eval_print newenv newtyenv
  with
    Failure str -> err_out str
  | Eval.Error str -> err_out str
  | Parsing.Parse_error -> err_out "Syntax Error"
  | Typing.Error str -> err_out str
  | _ -> err_out "Unknown Error" )

let initial_env =
  Environment.extend "i" (IntV 1)
  (Environment.extend "ii" (IntV 2)
   (Environment.extend "iii" (IntV 3)
    (Environment.extend "iv" (IntV 4)
      (Environment.extend "v" (IntV 5)
        (Environment.extend "x" (IntV 10) Environment.empty)))))

let initial_tyenv = 
  Environment.extend "i" (tysc_of_ty TyInt)
  (Environment.extend "ii" (tysc_of_ty TyInt)
    (Environment.extend "iii" (tysc_of_ty TyInt)
     (Environment.extend "iv" (tysc_of_ty TyInt)
       (Environment.extend "v" (tysc_of_ty TyInt)
         (Environment.extend "x" (tysc_of_ty TyInt) Environment.empty)))))

        
    