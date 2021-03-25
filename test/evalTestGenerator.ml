open Testutil
open Miniml.Eval

type evaluatedcase = { input: string; expected: exval }
type errorcase = { input: string }

type eval_to_raise_result =
  (* | Evaluated of exval *)
  | Evaluated of exval
  | ErrorRaised

(*original*)
(* let eval input =
  Exec.exec
    (fun env program ->
       let _, new_env, value = eval_decl env program in  
       new_env, value)
    Miniml.Cui.initial_env
    input *)

(* let eval_to_raise src =
  try
    let value = eval src in
    Evaluated value
  with
    Exec.Error msg -> raise (Exec.Error msg)
  | _ -> ErrorRaised *)

let get_right l = List.map(fun (_,v) -> v) l

(*new try*)
let eval input =
  Exec.exec
    (fun env program ->
       let (id_v, new_env) = eval_decl env program in 
       let (_,value) = List.hd id_v in
       new_env, value
      )
    Miniml.Cui.initial_env
    input

let eval_to_raise src =
  try
    let value = eval src in
    Evaluated value
  with
    Exec.Error msg -> raise (Exec.Error msg)
    | _ -> ErrorRaised


let eval_to_raise_result_printer = function
  | Evaluated value -> string_of_exval value
  | ErrorRaised -> "error"

let gen_eval_tests (dataset: evaluatedcase list) =
  gen_tests
    ~ishow: (fun x -> x)
    ~oshow: string_of_exval
    ~cmp: (=)
    ~exec: eval
  @@ List.map
    (fun (testcase: evaluatedcase): (string, exval) test ->
       { input = testcase.input; expected = testcase.expected })
    dataset

let gen_evalerror_tests (dataset: errorcase list) =
  gen_tests
    ~ishow: (fun x -> x)
    ~oshow: eval_to_raise_result_printer
    ~cmp: (=) (* TODO *)
    ~exec: eval_to_raise
  @@ List.map
    (fun (testcase: errorcase): (string, eval_to_raise_result) test ->
       { input = testcase.input; expected = ErrorRaised })
    dataset
