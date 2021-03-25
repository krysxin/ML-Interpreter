open OUnit
open TypingTestGenerator

let dataset_for_typing = [
  { input = "let rec id = fun x -> x in let rec f = fun y -> id (y id) in f;;"; expected = "(('a -> 'a) -> 'b) -> 'b" };
  { input = "let rec k = fun x -> fun y -> x in let rec k1 = fun x -> fun y -> k (x k) in k1;;"; expected = "(('a -> 'b -> 'a) -> 'c) -> 'd -> 'e -> 'c" };
  { input = "let rec s = fun x -> fun y -> fun z -> x z (y z) in let rec s1 = fun x -> fun y -> fun z -> x s (z s) (y s (z s)) in s1;;"; expected = "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('g -> 'h -> 'i) -> ('g -> 'h) -> 'g -> 'i) -> 'd -> 'e) -> ((('j -> 'k -> 'l) -> ('j -> 'k) -> 'j -> 'l) -> 'd) -> 'f" };
  { input = "let rec s = fun x -> fun y -> fun z -> x z (y z) in let rec k = fun x -> fun y -> x in s k k;;"; expected = "'a -> 'a" };
  { input = "let rec s = fun x -> fun y -> fun z -> x z (y z) in let rec k' = fun x -> fun y -> y in s k' k';;"; expected = "'a -> 'b -> 'b" };
  { input = "let rec pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let rec proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let rec proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (proj2 (pair 10 (pair 20 30)));;"; expected = "int" };
  { input = "let rec f = fun x -> x in if f true then f 1 else f 2;;"; expected = "int" };
  { input = "let rec f = fun x -> 3 in f true + f 4;;"; expected = "int" };
  { input = "fun b -> let rec f = fun x -> x in let rec g = fun y -> y in if b then f g else g f;;"; expected = "bool -> 'a -> 'a" };
  { input = "fun b -> fun f -> let rec g1 = fun x -> x f in let rec g2 = fun x -> x f in fun z -> if b then g1 z g2 else g2 z g1;;"; expected = "bool -> 'a -> ('a -> (('a -> 'b) -> 'b) -> 'c) -> 'c" };
  { input = "let rec apply = fun n -> fun f -> fun x -> if n < 1 then x else apply (n + -1) f (f x) in fun n -> if (apply n (fun x -> if x then false else true) true) then apply n (fun x -> x + 1) 0 else 0;;"; expected = "int -> int" };
  { input = "let rec id = fun x -> x;;\nlet rec f = fun y -> id (y id);;\nf;;"; expected = "(('a -> 'a) -> 'b) -> 'b" };
  { input = "let rec k = fun x -> fun y -> x;;\nlet rec k1 = fun x -> fun y -> k (x k);;\nk1;;"; expected = "(('a -> 'b -> 'a) -> 'c) -> 'd -> 'e -> 'c" };
  { input = "let rec s = fun x -> fun y -> fun z -> x z (y z);;\nlet rec s1 = fun x -> fun y -> fun z -> x s (z s) (y s (z s));;\ns1;;"; expected = "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('g -> 'h -> 'i) -> ('g -> 'h) -> 'g -> 'i) -> 'd -> 'e) -> ((('j -> 'k -> 'l) -> ('j -> 'k) -> 'j -> 'l) -> 'd) -> 'f" };
  { input = "let rec s = fun x -> fun y -> fun z -> x z (y z);;\nlet rec k = fun x -> fun y -> x;;\ns k k;;"; expected = "'a -> 'a" };
  { input = "let rec s = fun x -> fun y -> fun z -> x z (y z);;\nlet rec k' = fun x -> fun y -> y;;\ns k' k';;"; expected = "'a -> 'b -> 'b" };
  { input = "let rec pair = fun x1 -> fun x2 -> fun y -> y x1 x2;;\nlet rec proj1 = fun p -> p (fun x1 -> fun x2 -> x1);;\nlet rec proj2 = fun p -> p (fun x1 -> fun x2 -> x2);;\nproj1 (proj2 (pair 10 (pair 20 30)));;"; expected = "int" };
  { input = "let rec f = fun x -> x;;\nif f true then f 1 else f 2;;"; expected = "int" };
  { input = "let rec f = fun x -> 3;;\nf true + f 4;;"; expected = "int" };
  { input = "let rec apply = fun n -> fun f -> fun x -> if n < 1 then x else apply (n + -1) f (f x);;\nfun n -> if (apply n (fun x -> if x then false else true) true) then apply n (fun x -> x + 1) 0 else 0;;"; expected = "int -> int" };
];;

let () = ignore(run_test_tt_main (
    "ex4.4.2" >:::
    gen_typing_tests dataset_for_typing
  ))
