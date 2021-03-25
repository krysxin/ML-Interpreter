open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "(fun x -> x) 3;;"; expected = IntV 3 };
  { input = "(fun x -> fun y -> x) 3 4;;"; expected = IntV 3 };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z) in\n         let k = fun x -> fun y -> x in\n         s k k 3;;"; expected = IntV 3 };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z) ;;\n         let k = fun x -> fun y -> x;;\n         s k k 3;;" ; expected = IntV 3 };
  { input = "let f = fun x -> let x = 3 in x in f 5;;"; expected = IntV 3 };
  { input = "let f = fun x -> fun y -> let x = 3 in x+y in f 5 6;;"; expected = IntV 9 };
  { input = "let f = fun x -> let x = 3 in x;;\nf 5;;"; expected = IntV 3 };
  { input = "let f = fun x -> fun y -> let x = 3 in x+y;;\nf 5 6;;"; expected = IntV 9 };
  { input = "let f = fun x -> x in f f f f f f f 20;;"; expected = IntV 20 };
  { input = "let f = fun x -> x in f f (f f f f) f 20;;"; expected = IntV 20 };
];;

let () = ignore(run_test_tt_main (
    "ex3.4.1" >:::
    gen_eval_tests dataset_for_eval
  ))
