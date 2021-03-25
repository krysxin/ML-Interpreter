open OUnit
open PretypingTestGenerator
open Miniml

let dataset_for_unify = [
  {
    input = ["int", "int"];
    expected = UnifyOk []
  };
  {
    input = ["bool", "bool"];
    expected = UnifyOk []
  };
  {
    input = ["int", "bool"];
    expected = UnifyErr
  };
  {
    input = ["(int -> bool) -> int", "int -> bool -> int"];
    expected = UnifyErr
  };
  {
    input = ["int -> bool -> int", "int -> bool -> int"];
    expected = UnifyOk []
  };
  {
    input = ["'a", "int"];
    expected = UnifyOk ["'a", "int"]
  };
  {
    input = ["'a -> bool", "'b"];
    expected = UnifyOk ["'b", "'a -> bool"]
  };
  {
    input = ["'a", "'b"];
    expected = UnifyOk ["'a", "'b"]
  };
  {
    input = ["'a -> 'b -> bool", "int -> int -> bool"];
    expected = UnifyOk ["'a", "int"; "'b", "int"]
  };
  {
    input = ["'a -> 'b -> bool", "int -> int -> bool"];
    expected = UnifyOk ["'a", "int"; "'b", "int"]
  };
  {
    input = ["'a -> 'b -> 'a", "int -> bool -> int"];
    expected = UnifyOk ["'a", "int"; "'b", "bool"]
  };
  {
    input = ["'a -> 'b", "'b -> 'a"];
    expected = UnifyOk ["'a", "'b"]
  };
  {
    input = ["'a -> 'b -> 'b", "int -> bool -> int"];
    expected = UnifyErr
  };
  {
    input = ["('a -> 'b) -> 'a -> 'b", "'c -> int -> (bool -> int)"];
    expected = UnifyOk ["'c", "'a -> 'b"; "'a", "int"; "'b", "bool -> int"]
  };
  {
    input = ["('a -> 'b) -> 'a -> 'b", "'c -> int -> 'c"];
    expected = UnifyErr
  };
  {
    input = ["('a -> 'b) -> 'a -> 'b", "'c -> 'c"];
    expected = UnifyOk ["'c", "'a -> 'b"]
  };
  {
    input = ["'a -> 'b -> 'c -> 'd -> 'a", "('b -> 'b) -> ('c -> 'c) -> ('d -> 'd) -> ('e -> 'e) -> 'f"];
    expected =
      UnifyOk [
        "'a", "'b -> 'b"; "'b", "'c -> 'c"; "'c", "'d -> 'd"; "'d", "'e -> 'e";
        "'f", "((('e -> 'e) -> 'e -> 'e) -> ('e -> 'e) -> 'e -> 'e) -> (('e -> 'e) -> 'e -> 'e) -> ('e -> 'e) -> 'e -> 'e"
      ]
  };
  {
    input = ["'a -> 'b -> 'c -> 'd -> 'a", "('b -> 'b) -> ('c -> 'c) -> ('d -> 'd) -> ('e -> 'e) -> 'e"];
    expected = UnifyErr
  };
  {
    input = ["'a -> 'b -> 'c -> 'd -> 'a", "('b -> 'b) -> ('c -> 'c) -> ('d -> 'd) -> ('e -> 'e) -> (int -> bool)"];
    expected = UnifyErr
  };
  {
    input = ["'a -> (bool -> 'c)", "('b -> int) -> 'a"];
    expected = UnifyOk ["'a", "'b -> int"; "'b", "bool"; "'c", "int"]
  };
  {
    input = ["'a -> ('c -> bool)", "('b -> int) -> 'a"];
    expected = UnifyErr
  };
  {
    input = ["'a -> 'a", "'a"];
    expected = UnifyErr
  };
  {
    input = ["'a", "'a -> 'a"];
    expected = UnifyErr
  };
  {
    input = ["'a", "'b -> 'b"];
    expected = UnifyOk ["'a", "'b -> 'b"]
  };
  {
    input = ["'a -> 'a", "'b"];
    expected = UnifyOk ["'b", "'a -> 'a"]
  };
  {
    input = ["'a -> 'b", "('b -> 'b) -> 'a"];
    expected = UnifyErr
  };
  {
    input = ["'a -> 'c -> int -> 'c -> 'a", "'b -> int -> 'b -> int -> 'b"];
    expected = UnifyOk ["'a", "'b"; "'c", "int"; "'b", "int"]
  };
]

let () = ignore(run_test_tt_main (
    "ex4.3.3 - unify" >:::
    gen_unify_tests dataset_for_unify
  ))
