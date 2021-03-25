open OUnit

type ('a, 'b) test = { input: 'a; expected: 'b }

let gen_tests2 ?(cmp=(=)) ~errorhow ~exec tests =
  List.map
    (fun (test: ('a, 'b) test) ->
       "" >:: fun () ->
         let actual = exec test.input in
         assert_bool
           (errorhow test.input actual test.expected)
           (cmp actual test.expected)
    )
    tests

let gen_tests ?(cmp=(=)) ~ishow ~oshow ~exec tests =
  let errorhow input actual expected =
    Printf.sprintf
      "reason:    not equal\ninput:     %s\nactual:    %s\nexpected:  %s\n"
      (ishow input)
      (oshow actual)
      (oshow expected)
  in
  gen_tests2 ~cmp ~errorhow: errorhow ~exec tests
