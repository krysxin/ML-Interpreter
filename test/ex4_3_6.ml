open OUnit
open TypingTestGenerator

let dataset_for_typing = [
  { input = "let rec g = fun h -> fun t -> fun f -> fun x -> f h (t f x) in g;;"; expected = "'a -> (('a -> 'b -> 'c) -> 'd -> 'b) -> ('a -> 'b -> 'c) -> 'd -> 'c" };
  { input = "let rec s = fun x -> fun y -> fun z -> x z (y z) in let rec k = fun x -> fun y -> x in let rec k' = fun x -> fun y -> x in s k k';;"; expected = "'a -> 'a" };
  { input = "let rec pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let rec proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let rec proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (pair 1 100);;"; expected = "int" };
  { input = "let rec f = fun x -> f x in f;;"; expected = "'a -> 'b" };
  { input = "let rec f = fun x -> f (f x) in f;;"; expected = "'a -> 'a" };
  { input = "let rec fix_fun = fun g -> fun x -> g (fix_fun g) x in fix_fun;;"; expected = "(('a -> 'b) -> 'a -> 'b) -> 'a -> 'b" };
  { input = "fun f -> let rec x = fun z -> f (x z) in x 666;;"; expected = "('a -> 'a) -> 'a" };
  { input = "let rec f = fun x -> fun y -> if x < 0 then y else f (x + -1) y in f;;"; expected = "int -> 'a -> 'a" };
  { input = "fun f -> fun g -> let rec h = fun x -> h (g (f x)) in h;;"; expected = "('a -> 'b) -> ('b -> 'a) -> 'a -> 'c" };
  { input = "let rec loop = fun f -> fun x -> (loop f) (f x) in loop;;"; expected = "('a -> 'a) -> 'a -> 'b" };
  { input = "fun x -> let rec f = fun y -> x + 1 in x;;"; expected = "int -> int" };
  { input = "let rec ind = fun x -> fun f -> fun n -> if n < 1 then x else f (ind x f (n + -1)) in ind;;"; expected = "'a -> ('a -> 'a) -> int -> 'a" };
];;

let dataset_for_typingerror = [
  { input = "let rec f = fun x -> fun g -> g (x x g) in f f;;"; };
  { input = "let rec g = fun f -> fun x -> f x (f x) in g;;"; };
  { input = "let rec g = fun f -> fun x -> f x (x f) in g;;"; };
  { input = "fun b -> fun f -> let rec g1 = fun x -> f x in let rec g2 = fun x -> f x in if b then g1 g2 else g2 g1;;"; };
  { input = "let rec f = fun x -> f in f;;"; };
  { input = "let rec looq = fun f -> fun x -> (looq f) (x f) in looq;;"; };
  { input = "let rec f = fun x -> f (x f) in f;;"; };
  { input = "let rec f = fun z -> f z (fun g -> fun h -> h (g h)) in f;;"; };
];;

let () = ignore(run_test_tt_main (
    "ex4.3.6" >:::
    gen_typing_tests dataset_for_typing
    @ gen_typingerror_tests dataset_for_typingerror
  ))
