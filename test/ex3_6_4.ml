open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "match [1; 2; 3] with [] -> 3 | x :: y :: rest -> 5 | x::rest -> 4 ;;"; expected = IntV 5 };
  { input = "match [ [1]; [2]; [3] ] with [] -> 3\n         | [2] :: rest -> rest\n         | [1] :: x :: rest -> x\n         | x -> x ;;"; expected = ConsV (IntV 2, NilV) };
  { input = "match [] with [] -> 0 | rest -> rest;;"; expected = IntV 0 };
  { input = "match [10] with [] -> 0 | rest -> rest;;"; expected = ConsV (IntV 10, NilV) };
  { input = "match [[30; 20]; [10; 40]] with (x :: y) :: rest -> x | rest -> 0;;"; expected = IntV 30 };
  { input = "match [[30; 20]; [10; 40]] with (x :: y) :: [] -> x | rest -> 0;;"; expected = IntV 0 };
  { input = "match [[30; 20]; [10; 40]] with (x :: y) :: (z :: w) :: rest -> w | rest -> [];;"; expected = ConsV (IntV 40, NilV) };
];;

let () = ignore(run_test_tt_main (
    "ex3.6.4" >:::
    gen_eval_tests dataset_for_eval
  ))
