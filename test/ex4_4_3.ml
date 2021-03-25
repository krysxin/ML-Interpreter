open OUnit
open TypingTestGenerator

let dataset_for_typing = [
  { input = "(1 : int);;"; expected = "int" };
  { input = "(true : bool);;"; expected = "bool" };
  { input = "(fun x -> x : 'a -> int);;"; expected = "int -> int" };
  { input = "fun (x : int) -> fun y -> y x;;"; expected = "int -> (int -> 'a) -> 'a" };
  { input = "fun (x : 'a -> 'b) -> x;;"; expected = "('a -> 'b) -> 'a -> 'b" };
  { input = "(fun x -> x : 'a -> 'b);;"; expected = "'a -> 'a" };
];;

let dataset_for_typingerror = [
  { input = "(1 : bool);;"; };
];;

let () = ignore(run_test_tt_main (
    "ex4.4.3" >:::
    gen_typing_tests dataset_for_typing
    @ gen_typingerror_tests dataset_for_typingerror
  ))
