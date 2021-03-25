open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "[1; 2; 3];;"; expected = ConsV (IntV 1, ConsV (IntV 2, ConsV (IntV 3, NilV))) };
  { input = "4 :: [1; 2; 3];;"; expected = ConsV (IntV 4, ConsV (IntV 1, ConsV (IntV 2, ConsV (IntV 3, NilV)))) };
  { input = "[1] :: [2] :: [];;"; expected = ConsV (ConsV (IntV 1, NilV), ConsV (ConsV (IntV 2, NilV), NilV)) };
  { input = "[1] :: [2] :: [] :: [];;"; expected = ConsV ((ConsV (IntV 1, NilV)), ConsV ((ConsV (IntV 2, NilV)), ConsV (NilV, NilV))) };
  { input = "[[1]; [1; 2]; 3];;"; expected = ConsV (ConsV (IntV 1, NilV), ConsV (ConsV (IntV 1, ConsV (IntV 2, NilV)), ConsV (IntV 3, NilV))) };
];;

let () = ignore(run_test_tt_main (
    "ex3.6.2" >:::
    gen_eval_tests dataset_for_eval
  ))
