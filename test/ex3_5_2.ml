open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let rec f = fun x -> x and g = fun y -> y in f 3;;"; expected = IntV 3 };
  { input = "let rec f = fun x -> x and g = fun y -> y;;\nf 3;;"; expected = IntV 3 };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         odd 3;;"; expected = BoolV true };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         even 3;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         odd 4;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         even 4;;"; expected = BoolV true };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         odd 3;;"; expected = BoolV true };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         even 3;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         odd 4;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         even 4;;"; expected = BoolV true };
];;

let () = ignore(run_test_tt_main (
    "ex3.5.2" >:::
    gen_eval_tests dataset_for_eval
  ))
