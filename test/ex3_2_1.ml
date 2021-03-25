open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "1 + 2;;"; expected = IntV 3 };
  { input = "-2 * 2;;"; expected = IntV (-4) };
  { input = "if true then 3 else 4;;"; expected = IntV 3 };
  { input = "if false then 3 else 4;;"; expected = IntV 4 };
  { input = "3 + 4 * 5;;"; expected = IntV 23 };
  { input = "3 * 4 + 5;;"; expected = IntV 17 };
  { input = "(3 + 4) * 5;;"; expected = IntV 35 };
  { input = "(if 25 < 16 then 91 else 83 + 40) < 113;;"; expected = BoolV false };
  { input = "if if true then true else true then false else true;;"; expected = BoolV false };
  { input = "(23 + (90 + 100)) * (if 13 < 31 then 40 else 40);;"; expected = IntV 8520 };
  { input = "108 < 46 + (119 + (if true then 92 else 12) + 6);;"; expected = BoolV true };
  { input = "10 * 20 + 1 < 34 + 10 * 9;;"; expected = BoolV false };
  { input = "if true then 34 else a;;"; expected = IntV 34 };
  { input = "if false then a else false;;"; expected = BoolV false };
  { input = "((((((((((((((((((((1))))))))))))))))))));;"; expected = IntV 1 };
];;

let dataset_for_evalerror = [
  { input = "1 < true;;" };
  { input = "false < true;;" };
  { input = "false < 1;;" };
  { input = "false + 34;;" };
  { input = "true * 3;;" };
  { input = "if 0 then 34 else 5;;" };
  { input = "a;;" };
  { input = "if a + 19 < 0 then true else false;;" };
];;

let () = ignore(run_test_tt_main (
    "ex3.2.1" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))
