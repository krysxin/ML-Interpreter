open OUnit
open TypingTestGenerator
open Miniml.Syntax

let dataset_for_typing = [
  { input = "fun x -> x;;"; expected = "'a -> 'a" };
  { input = "fun x -> fun y -> x;;"; expected = "'a -> 'b -> 'a" };
  { input = "fun x -> fun y -> y;;"; expected = "'a -> 'b -> 'b" };
  { input = "fun xa -> fun xb -> fun xc -> fun xd -> fun xe -> fun xf -> fun xg -> fun xh -> fun xi -> fun xj -> fun xk -> fun xl -> fun xm -> fun xn -> fun xo -> fun xp -> fun xq -> fun xr -> fun xs -> fun xt -> fun xu -> fun xv -> fun xw -> fun xx -> fun xy -> fun xz -> fun ya -> fun yb -> fun yc -> fun yd -> fun ye -> fun yf -> fun yg -> fun yh -> fun yi -> fun yj -> fun yk -> fun yl -> fun ym -> fun yn -> fun yo -> fun yp -> fun yq -> fun yr -> fun ys -> fun yt -> fun yu -> fun yv -> fun yw -> fun yx -> fun yy -> fun yz -> 0;;"; expected = "'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> 'a1 -> 'b1 -> 'c1 -> 'd1 -> 'e1 -> 'f1 -> 'g1 -> 'h1 -> 'i1 -> 'j1 -> 'k1 -> 'l1 -> 'm1 -> 'n1 -> 'o1 -> 'p1 -> 'q1 -> 'r1 -> 's1 -> 't1 -> 'u1 -> 'v1 -> 'w1 -> 'x1 -> 'y1 -> 'z1 -> int" };
  { input = "(fun x -> x + 1) 2 + (fun x -> x + -1) 3;;"; expected = "int" };
  { input = "fun f -> fun g -> fun x -> g (f x);;"; expected = "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c" };
  { input = "fun x -> fun y -> fun z -> x z (y z);;"; expected = "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c" };
  { input = "fun x -> let y = x + 1 in x;;"; expected = "int -> int" };
  { input = "fun x -> let y = x + 1 in y;;"; expected = "int -> int" };
  { input = "fun b -> fun x -> if x b then x else (fun x -> b);;"; expected = "bool -> (bool -> bool) -> bool -> bool " };
  { input = "fun x -> if true then x else (if x then true else false);;"; expected = "bool -> bool" };
  { input = "fun x -> fun y -> if x then x else y;;"; expected = "bool -> bool -> bool" };
  { input = "fun n -> (fun x -> x (fun y -> y)) (fun f -> f n);;"; expected = "'a -> 'a" };
  { input = "fun x -> fun y -> x y;;"; expected = "('a -> 'b) -> 'a -> 'b" };
  { input = "fun x -> fun y -> x (y x);;"; expected = "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b" };
  { input = "fun x -> fun y -> x (y x) (y x);;"; expected = "('a -> 'a -> 'b) -> (('a -> 'a -> 'b) -> 'a) -> 'b" };
  { input = "fun x -> fun y -> fun z -> x (z x) (y (z x y));;"; expected = "((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> (((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> 'a) -> 'c" };
  { input = "let g = fun h -> fun t -> fun f -> fun x -> f h (t f x) in g;;"; expected = "'a -> (('a -> 'b -> 'c) -> 'd -> 'b) -> ('a -> 'b -> 'c) -> 'd -> 'c" };
  { input = "let s = fun x -> fun y -> fun z -> x z (y z) in let k = fun x -> fun y -> x in let k' = fun x -> fun y -> x in s k k';;"; expected = "'a -> 'a" };
  { input = "fun x -> fun y -> fun z -> let b = x y z in if b then z y else y;;"; expected = "('a -> ('a -> 'a) -> bool) -> 'a -> ('a -> 'a) -> 'a" };
  { input = "let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in\n       let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (pair 1 100);;"; expected = "int" };
];;

let dataset_for_typingerror = [
  { input = "1 + true;;"; };
  { input = "2 + (fun x -> x);;"; };
  { input = "-2 * false;;"; };
  { input = "fun x -> x x;;"; };
  { input = "let f = fun x -> fun g -> g (x x g) in f f;;"; };
  { input = "let g = fun f -> fun x -> f x (f x) in g;;"; };
  { input = "let g = fun f -> fun x -> f x (x f) in g;;"; };
  { input = "fun x -> fun y -> x y + y x;;"; };
  { input = "fun x -> fun y -> x y + x;;"; };
  { input = "fun x -> fun y -> if x y then x else y;;"; };
  { input = "fun x -> fun y -> if x y then (fun z -> if y z then z else x) else (fun x -> x);;"; };
  { input = "fun x -> fun y -> fun z -> let b = x y z in if b then z y else z x;;"; };
  { input = "fun x -> fun y -> fun z -> if x y then z x else y z;;"; };
  { input = "fun x -> if x then 1 else x;;"; };
  { input = "(fun x -> x + 1) true;;"; };
  { input = "fun x -> fun y -> y (x (y x));;"; };
  { input = "(fun f -> fun x -> f (f x)) (fun x -> fun y -> x);;"; };
  { input = "fun x -> fun y -> y (x (fun z1 -> fun z2 -> z1)) (x (fun z -> z));;"; };
  { input = "fun b -> fun f -> let g1 = fun x -> f x in let g2 = fun x -> f x in if b then g1 g2 else g2 g1;;"; };
];;

let () = ignore(run_test_tt_main (
    "ex4.3.5" >:::
    gen_typing_tests dataset_for_typing
    @ gen_typingerror_tests dataset_for_typingerror
  ))
