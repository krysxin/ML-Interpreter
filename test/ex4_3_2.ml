open OUnit
open PretypingTestGenerator
open Miniml

let dataset_for_subst_type = [
  (* 1st example in the textbook *)
  {
    subst = ["'a", "int"];
    input = "'a -> bool";
    expected = "int -> bool"
  };
  (* 2nd example in the textbook *)
  {
    subst = [
      "'b", "'a -> int";
      "'a", "bool"
    ];
    input = "'b";
    expected = "bool -> int"
  };
  {
    subst = [];
    input = "int";
    expected = "int"
  };
  {
    subst = ["'a", "int"];
    input = "bool";
    expected = "bool"
  };
  {
    subst = ["'a", "int"];
    input = "'a";
    expected = "int"
  };
  {
    subst = ["'b", "'a -> 'a"];
    input = "'a -> 'b";
    expected = "'a -> 'a -> 'a"
  };
  {
    subst = [
      "'a", "'b";
      "'b", "int -> int"
    ];
    input = "'a -> 'a";
    expected = "(int -> int) -> int -> int"
  };
  {
    subst = [
      "'a", "'b";
      "'b", "int -> int"
    ];
    input = "int -> (int -> bool) -> bool";
    expected = "int -> (int -> bool) -> bool"
  };
  {
    subst = [
      "'b", "int -> 'a -> int";
      "'a", "bool -> bool"
    ];
    input = "'a -> 'b -> 'a";
    expected = "(bool -> bool) -> (int -> (bool -> bool) -> int) -> bool -> bool"
  };
  {
    subst = ["'a", "'a"];
    input = "'a -> 'b -> 'a";
    expected = "'a -> 'b -> 'a"
  };
  {
    subst = [
      "'a", "'a -> 'a";
      "'a", "'a -> 'a";
      "'a", "'a -> 'a -> 'a";
    ];
    input = "'a";
    expected = "(('a -> 'a -> 'a) -> 'a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a -> 'a"
  };
  {
    subst = [
      "'a", "'c";
      "'b", "'a";
      "'c", "'b"
    ];
    input = "'a -> 'b -> 'a";
    expected = "'b -> 'a -> 'b"
  };
  {
    subst = [
      "'a", "'b"; "'b", "'c"; "'c", "'d"; "'d", "'e"; "'e", "'f";
      "'f", "'g"; "'g", "'h"; "'h", "'i"; "'i", "'j"; "'j", "'k";
      "'k", "'l"; "'l", "'m"; "'m", "'n"; "'n", "'o"; "'o", "'p";
      "'p", "'q"; "'q", "'r"; "'r", "'s"; "'s", "'t"; "'t", "'u";
      "'u", "'v"; "'v", "'w"; "'w", "'x"; "'x", "'y"; "'y", "'z";
      "'z", "'a1"; "'a1", "'b1"; "'b1", "'c1"; "'c1", "'d1"; "'d1", "'e1";
      "'e1", "'f1"; "'f1", "'g1";
    ];
    input = "'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> 'a1 -> 'b1 -> 'c1 -> 'd1 -> 'e1";
    expected = "'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1 -> 'g1"
  };
  {
    subst = [
      "'b", "'c -> 'a -> 'c";
      "'c", "int -> 'a";
      "'a", "bool"
    ];
    input = "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b";
    expected = "(bool -> ((int -> bool) -> bool -> int -> bool)) -> ((bool -> (int -> bool) -> bool -> int -> bool) -> bool) -> (int -> bool) -> bool -> int -> bool"
  };
  {
    subst = [
      "'a", "int";
      "'a", "'b";
      "'b", "'a";
      "'a", "bool";
      "'a", "bool -> int";
      "'b", "int -> bool";
    ];
    input = "'a -> 'b";
    expected = "int -> bool";
  };
  {
    subst = [
      "'a", "'c"; "'c", "'d"; "'d", "int -> int";
      "'b", "'d"; "'d", "bool -> bool"
    ];
    input = "'a -> 'b";
    expected = "(int -> int) -> bool -> bool";
  };
  {
    subst = [
      "'a", "'b -> 'b";
      "'b", "'c -> 'c";
      "'c", "'d -> 'd";
      "'d", "'e -> 'e";
      "'e", "'f -> 'f";
    ];
    input = "'b";
    expected = "((('f -> 'f) -> 'f -> 'f) -> ('f -> 'f) -> 'f -> 'f) -> (('f -> 'f) -> 'f -> 'f) -> ('f -> 'f) -> 'f -> 'f";
  };
  {
    subst = [
      "'c", "'d";
      "'d", "'e";
      "'e", "'f";
      "'f", "'g";
      "'g", "int -> 'a -> bool -> 'b";
    ];
    input = "bool -> ('a -> 'a) -> 'b";
    expected = "bool -> ('a -> 'a) -> 'b"
  };
]

let () = ignore(run_test_tt_main (
    "ex4.3.2 - subst_type" >:::
    gen_subst_type_tests dataset_for_subst_type
  ))
