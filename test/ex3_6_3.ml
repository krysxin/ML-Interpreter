open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_evalerror = [
  { input = "let rec length = fun l -> match l with [] -> 0\n         | x::x -> 1 + length x in length (1 :: 2 :: 3 :: []);;"; };
  { input = "match 1 :: 2 :: [] with [] -> 0 | x :: x -> x;;" };
];;

let () = ignore(run_test_tt_main (
    "ex3.6.3" >:::
    gen_evalerror_tests dataset_for_evalerror
  ))
