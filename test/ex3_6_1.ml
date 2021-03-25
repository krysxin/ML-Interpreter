open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "1 :: 2 :: 3 :: [];;"; expected = ConsV (IntV 1, ConsV (IntV 2, ConsV (IntV 3, NilV))) };
  { input = "(1 :: []) :: (1 :: 2 :: []) :: 3 :: [];;"; expected = ConsV (ConsV (IntV 1, NilV), ConsV (ConsV (IntV 1, ConsV (IntV 2, NilV)), ConsV (IntV 3, NilV))) };
  { input = "(fun x -> x :: x :: []) 4;;"; expected = ConsV (IntV 4, ConsV (IntV 4, NilV)) };
  { input = "let rec length = fun l -> match l with [] -> 0\n         | x::rest -> 1 + length rest in length (1 :: 2 :: 3 :: []);;"; expected = IntV 3 };
  { input = "let f = fun cons -> match cons with [] -> 0 | x :: y -> x in (f []) :: (f (1 :: [])) :: [];;"; expected = ConsV (IntV 0, ConsV (IntV 1, NilV)) };
  { input = "let x = [] in 0 :: x;;"; expected = ConsV (IntV 0, NilV) };
  { input = "let rec append = fun a -> fun b -> match a with [] -> b | hd :: tl -> hd :: append tl b in append (1 :: 2 :: 3 :: []) (4 :: 5 :: 6 :: []);;"; expected = ConsV (IntV 1, ConsV (IntV 2, ConsV (IntV 3, ConsV (IntV 4, ConsV (IntV 5, ConsV (IntV 6, NilV)))))) };
];;

let () = ignore(run_test_tt_main (
    "ex3.6.1" >:::
    gen_eval_tests dataset_for_eval
  ))
