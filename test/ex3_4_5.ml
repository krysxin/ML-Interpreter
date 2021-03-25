open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let a = 3 in let p = dfun x -> x + a in let a = 5 in a * p 2;;"; expected = IntV 35};
  { input = "let f = dfun x -> if x < 100 then f (x + 3) else x in f 0;;"; expected = IntV 102};
  { input = "let f = dfun x -> x + a in let a = 9 in let f = fun x -> f x in let a = 13 in f 8;;"; expected = IntV 17};
];;

let () = ignore(run_test_tt_main (
    "ex3.4.5" >::: 
    gen_eval_tests dataset_for_eval
  ))
