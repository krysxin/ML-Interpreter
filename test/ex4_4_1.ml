open OUnit
open TypingTestGenerator

let dataset_for_typing = [
  { input = "let id = fun x -> x in let f = fun y -> id (y id) in f;;"; expected = "(('a -> 'a) -> 'b) -> 'b" };
  { input = "let k = fun x -> fun y -> x in let k1 = fun x -> fun y -> k (x k) in k1;;"; expected = "(('a -> 'b -> 'a) -> 'c) -> 'd -> 'e -> 'c" };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z) in let s1 = fun x -> fun y -> fun z -> x s (z s) (y s (z s)) in s1;;"; expected = "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('g -> 'h -> 'i) -> ('g -> 'h) -> 'g -> 'i) -> 'd -> 'e) -> ((('j -> 'k -> 'l) -> ('j -> 'k) -> 'j -> 'l) -> 'd) -> 'f" };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z) in let k = fun x -> fun y -> x in s k k;;"; expected = "'a -> 'a" };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z) in let k' = fun x -> fun y -> y in s k' k';;"; expected = "'a -> 'b -> 'b" };
  { input = "let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (proj2 (pair 10 (pair 20 30)));;"; expected = "int" };
  { input = "let f = fun x -> x in if f true then f 1 else f 2;;"; expected = "int" };
  { input = "let f = fun x -> 3 in f true + f 4;;"; expected = "int" };
  { input = "fun b -> let f = fun x -> x in let g = fun y -> y in if b then f g else g f;;"; expected = "bool -> 'a -> 'a" };
  { input = "fun b -> fun f -> let g1 = fun x -> x f in let g2 = fun x -> x f in fun z -> if b then g1 z g2 else g2 z g1;;"; expected = "bool -> 'a -> ('a -> (('a -> 'b) -> 'b) -> 'c) -> 'c" };
  { input = "let id = fun x -> x;;\nlet f = fun y -> id (y id);;\nf;;"; expected = "(('a -> 'a) -> 'b) -> 'b" };
  { input = "let k = fun x -> fun y -> x;;\nlet k1 = fun x -> fun y -> k (x k);;\nk1;;"; expected = "(('a -> 'b -> 'a) -> 'c) -> 'd -> 'e -> 'c" };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z);;\nlet s1 = fun x -> fun y -> fun z -> x s (z s) (y s (z s));;\ns1;;"; expected = "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('g -> 'h -> 'i) -> ('g -> 'h) -> 'g -> 'i) -> 'd -> 'e) -> ((('j -> 'k -> 'l) -> ('j -> 'k) -> 'j -> 'l) -> 'd) -> 'f" };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z);;\nlet k = fun x -> fun y -> x;;\ns k k;;"; expected = "'a -> 'a" };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z);;\nlet k' = fun x -> fun y -> y;;\ns k' k';;"; expected = "'a -> 'b -> 'b" };
  { input = "let pair = fun x1 -> fun x2 -> fun y -> y x1 x2;;\nlet proj1 = fun p -> p (fun x1 -> fun x2 -> x1);;\nlet proj2 = fun p -> p (fun x1 -> fun x2 -> x2);;\nproj1 (proj2 (pair 10 (pair 20 30)));;"; expected = "int" };
  { input = "let f = fun x -> x;;\nif f true then f 1 else f 2;;"; expected = "int" };
  { input = "let f = fun x -> 3;;\nf true + f 4;;"; expected = "int" };
];;

let () = ignore(run_test_tt_main (
    "ex4.4.1" >:::
    gen_typing_tests dataset_for_typing
  ))
