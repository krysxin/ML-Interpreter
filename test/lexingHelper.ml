let from_string_without_eof str =
  let pos = ref 0 in
  Lexing.from_function
  @@ fun res n ->
  let npos = min (String.length str) (!pos + n) in
  let read_count = npos - !pos in
  if read_count = 0 then
    raise End_of_file
  else
    let ppos = !pos in
    (while !pos < npos do
       Bytes.set res (!pos - ppos) (String.get str !pos);
       pos := !pos + 1;
     done;
     read_count)
