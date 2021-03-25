open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let x = 10;;\n let x = 100 and y = x in x + y;;"; expected = IntV 110 };
  { input = "let x = 10;;\n let x = 100 and y = x;;\n x;;"; expected = IntV 100 };
  { input = "let x = 10;;\n let x = 100 and y = x;;\n y;;"; expected = IntV 10 };
  { input = "let x = 3 and y = 5 in x + y;;"; expected = IntV 8 };
  { input = "let z = let x = 3 and y = 5 in x + y;;\nz;;"; expected = IntV 8 };
  { input = "let x = 1 and y = 2 in let z = x and w = y in z + w;;"; expected = IntV 3 };
];;

let dataset_for_evalerror = [
  { input = "let a = 1 and a = 1;; a;;"; };
  { input = "let b = 1 and b = 1 in b + 5;;"; };
];;

let () = ignore(run_test_tt_main (
    "ex3.3.4" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))
