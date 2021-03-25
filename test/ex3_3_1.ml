open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let x = 3 in x;;"; expected = IntV 3 };
  { input = "let x = 3 in let y = 4 in x+y;;"; expected = IntV 7 };
  { input = "let x = 3 in let y = 4 in let x = 5 in x+y;;"; expected = IntV 9 };
  { input = "let x = 3;;\nx;;"; expected = IntV 3 };
  { input = "let x = let y = 5 in 3+y;;\nx;;"; expected = IntV 8 };
  { input = "let x = let y = 5 in let z = 3 in y+z;;\nx;;"; expected = IntV 8 };
  { input = "let x = 3 in let x = x + 5 in x;;"; expected = IntV 8 };
];;

let dataset_for_evalerror = [
  { input = "let x = let y = 5 in 3+y;;\n y;;"; };
  { input = "let x = let y = 5 in let z = 3 in y+z;;\n y;;"; };
];;

let () = ignore(run_test_tt_main (
    "ex3.3.1" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))
