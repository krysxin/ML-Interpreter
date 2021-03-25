open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "true && false;;"; expected = BoolV false };
  { input = "true || false;;"; expected = BoolV true };
  { input = "true || false && false;;"; expected = BoolV true };
  { input = "true && false || true;;"; expected = BoolV true };
  { input = "1 < 2 && false;;"; expected = BoolV false };
  { input = "if 1 < 2 && 3 < 4 then true || false else true && false;;"; expected = BoolV true };
  { input = "if 1 < 2 && 3 < 1 then true || false else true && false;;"; expected = BoolV false};
  { input = "false && undef;;"; expected = BoolV false };
  { input = "true || undef;;"; expected = BoolV true };
];;

let () = ignore(run_test_tt_main (
    "ex3.2.3" >:::
    gen_eval_tests dataset_for_eval
  ))
