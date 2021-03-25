open OUnit
open TypingTestGenerator
open Miniml.Syntax

let dataset_for_typing = [
  { input = "1 + 2;;"; expected = "int" };
  { input = "-2 * 2;;"; expected = "int" };
  { input = "1 < 2;;"; expected = "bool" }; 
  { input = "let x = 1 in let y = true in x;;"; expected = "int" };
  { input = "let x = 1 in let y = true in y;;"; expected = "bool" };
];;

let () = ignore(run_test_tt_main (
    "ex4.2.1" >:::
    gen_typing_tests dataset_for_typing
  ))
