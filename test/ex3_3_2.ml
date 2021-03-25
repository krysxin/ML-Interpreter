open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let x = 1 let y = x + 1;;\n x;;"; expected = IntV 1 };
  { input = "let x = 1 let y = x + 1;;\n y;;"; expected = IntV 2 };
  { input = "let x = 1 let y = x + 1 let z = x + y;;\n z;;"; expected = IntV 3 };
  { input = "let x = 1 let y = x + 1 let x = 5;;\n x;;"; expected = IntV 5 };
  { input = "let x = 1 let y = let x = 3 in x + 1;;\n y;;"; expected = IntV 4 };
];;

let () = ignore(run_test_tt_main (
    "ex3.3.2" >:::
    gen_eval_tests dataset_for_eval
  ))
