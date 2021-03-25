open OUnit
open TypingTestGenerator

let dataset_for_typing = [
  { input = "[];;"; expected = "'a list"; };
  { input = "[] :: [];;"; expected = "'a list list"; };
  { input = "fun x -> fun y -> x :: y;;"; expected = "'a -> 'a list -> 'a list"; };
  { input = "fun x -> fun f -> f (f x :: []);;"; expected = "'a list -> ('a list -> 'a) -> 'a"; };
  { input = "fun x -> match x with [] -> 0 | h :: t -> h;;"; expected = "int list -> int"; };
];;

let dataset_for_typingerror = [
  { input = "3 :: true :: [];;"; };
  { input = "fun x -> x :: x;;"; };
  { input = "fun x -> fun y -> x :: x y;;"; };
  { input = "fun x -> fun y -> fun z -> x y :: z x :: y z :: [];;"; };
  { input = "fun x -> match x with [] -> 0 | h :: t -> x :: t;;"; };
  { input = "fun x -> match x with [] -> 0 | h :: t -> (h 1) + (h true);;"; };
];;

let () = ignore(run_test_tt_main (
    "ex4.3.7" >:::
    gen_typing_tests dataset_for_typing
    @ gen_typingerror_tests dataset_for_typingerror
  ))
