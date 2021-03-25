open OUnit
open PretypingTestGenerator
open Miniml

let dataset_for_string_of_ty = [
  { input = "int"; };
  { input = "bool"; };
  { input = "'a"; };
  { input = "int -> int"; };
  { input = "int -> (int -> bool) -> bool"; };
  { input = "'a -> 'b"; };
  { input = "'a -> 'a"; };
  { input = "'a -> 'b -> 'a"; };
  { input = "'a -> 'b -> 'b"; };
  { input = "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b"; };
  { input = "'a -> ('a -> 'a) -> int -> 'a"; };
  { input = "bool -> 'a -> 'a"; };
  { input = "'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> 'a1 -> 'b1 -> 'c1 -> 'd1 -> 'e1 -> 'f1 -> 'g1 -> 'h1 -> 'i1 -> 'j1 -> 'k1 -> 'l1 -> 'm1 -> 'n1 -> 'o1 -> 'p1 -> 'q1 -> 'r1 -> 's1 -> 't1 -> 'u1 -> 'v1 -> 'w1 -> 'x1 -> 'y1 -> 'z1 -> int"; };
]

let (dataset_for_freevar_ty: freevar_ty_testcase list) = [
  { input = "int"; expected = MySet.from_list [] };
  { input = "bool"; expected = MySet.from_list [] };
  { input = "'a"; expected = MySet.from_list [0] };
  { input = "'a -> 'b"; expected = MySet.from_list [0; 1] };
  { input = "'a -> 'a"; expected = MySet.from_list [0] };
  { input = "int -> int"; expected = MySet.from_list [] };
  { input = "int -> (int -> bool) -> bool"; expected = MySet.from_list [] };
  { input = "'a -> 'b -> 'a"; expected = MySet.from_list [0; 1] };
  { input = "'a -> 'b -> 'b"; expected = MySet.from_list [0; 1] };
  { input = "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b"; expected = MySet.from_list [0; 1] };
  { input = "'a -> ('a -> 'a) -> int -> 'a"; expected = MySet.from_list [0] };
  { input = "bool -> 'a -> 'a"; expected = MySet.from_list [0] };
  {
    input = "'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> 'a1 -> 'b1 -> 'c1 -> 'd1 -> 'e1 -> 'f1 -> 'g1 -> 'h1 -> 'i1 -> 'j1 -> 'k1 -> 'l1 -> 'm1 -> 'n1 -> 'o1 -> 'p1 -> 'q1 -> 'r1 -> 's1 -> 't1 -> 'u1 -> 'v1 -> 'w1 -> 'x1 -> 'y1 -> 'z1 -> int";
    expected = MySet.from_list (List.init 52 (fun x -> x))
  };
]

let () = ignore(run_test_tt_main (
    "ex4.3.1 - string_of_ty" >:::
    gen_string_of_ty_tests dataset_for_string_of_ty
  ))

let () = ignore(run_test_tt_main (
    "ex4.3.1 - freevar_ty" >:::
    gen_freevar_ty_tests dataset_for_freevar_ty
  ))
