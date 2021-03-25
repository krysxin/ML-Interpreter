open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes (+) 5;;"; expected = IntV 20};
  { input = "let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes ( * ) 5;;"; expected = IntV 625};
  { input = "let exlt = fun f -> f 2 3 in exlt (<) ;;"; expected = BoolV true};
  { input = "let exand = fun f -> f true false in exand (&&) ;;"; expected = BoolV false};
  { input = "let exor = fun f -> f true false in exor (||) ;;"; expected = BoolV true};
  { input = "let threetimes = fun f -> fun x -> fun y -> fun z -> f (f x y) (f y z) in threetimes (||) false false (3<4);;"; expected = BoolV true}
];;


let () = ignore(run_test_tt_main (
    "ex3.4.2" >:::
    gen_eval_tests dataset_for_eval
  ))