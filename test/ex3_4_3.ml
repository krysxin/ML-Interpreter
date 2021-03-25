open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "(fun x y -> x) 3 4;;"; expected = IntV 3 };
  { input = "let s = fun x y z -> x z (y z) in\n         let k = fun x y -> x in\n         s k k 3;;"; expected = IntV 3 };
  { input = "let s x y z = x z (y z) in\n         let k x y = x in\n         s k k 3;;"; expected = IntV 3 };
  { input = "let s = fun x y z -> x z (y z) ;;\n         let k = fun x y -> x;;\n         s k k 3;;"; expected = IntV 3 };
  { input = "let s x y z = x z (y z) ;;\n         let k x y = x;;\n         s k k 3;;"; expected = IntV 3 };
];;

let dataset_for_evalerror = [
  { input = "let f x y z -> 10 in f 0 0 0;;" };
  { input = "fun x y z = 10;;" };
  { input = "let f x y z -> 10;;" };
];;

let () = ignore(run_test_tt_main (
    "ex3.4.3" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))
