open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "1 + if true then 2 else 3;;"; expected = IntV 3 };
  { input = "if true then 2 else 3 + 1;;"; expected = IntV 2 };
  { input = "0 < if true then 2 else 3;;"; expected = BoolV true };
];;

let () = ignore(run_test_tt_main (
    "ex3.6.5" >:::
    gen_eval_tests dataset_for_eval
  ))
