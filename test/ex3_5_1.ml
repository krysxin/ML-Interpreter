open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let rec f = fun x -> x in f 3;;"; expected = IntV 3 };
  { input = "let rec f = fun x -> x;;\nf 3;;"; expected = IntV 3 };
  { input = "let rec fact = fun n -> if n < 1 then 1 else n*fact (n + -1)\n         in fact 5;;"; expected = IntV 120 };
  { input = "let rec fact = fun n -> if n < 1 then 1 else n*fact (n + -1);;\n         fact 5;;"; expected = IntV 120 };
  { input = "let rec ack = fun m -> fun n -> if m < 1 then n + 1 else if n < 1 then ack (m + (-1)) 1 else ack (m + (-1)) (ack m (n + (-1))) in ack 3 3;;"; expected = IntV 61 };
];;

let () = ignore(run_test_tt_main (
    "ex3.5.1" >:::
    gen_eval_tests dataset_for_eval
  ))
