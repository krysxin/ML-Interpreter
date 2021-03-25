open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "(* comment *) 0;;"; expected = IntV 0 };
  { input = "0 (* comment *);;"; expected = IntV 0 };
  { input = "(* comment (* nest comment *) *) 0;;"; expected = IntV 0};
  { input = "(*) seems times operator comment *) 0;;"; expected = IntV 0 };
  { input = "10 (* comment 1 *) + (* comment 2 *) 9;;"; expected = IntV 19 };
  { input = "(**)if(**)true(**)then(**)1(**)else(**)2(**);;"; expected = IntV 1 };
  { input = "if (* if 1 < 2 then true else *) false then 1 else 2;;"; expected = IntV 2 };
];;

let dataset_for_evalerror = [
  { input = "0 ( * this is not a comment * );;" };
];;

let () = ignore(run_test_tt_main (
    "ex3.2.4" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))
