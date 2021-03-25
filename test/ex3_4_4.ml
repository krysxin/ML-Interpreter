
(*draft of ex3_4_4
Original: 4+4+4=12
let makemult = fun maker -> fun x ->
                 if x < 1 then 0 else 4 + maker maker (x + -1) in
let times4 = fun x -> makemult makemult x in 
  times4 3

ver1: 4*4*4=64
let makemult = fun maker -> fun x ->
                 if x < 1 then 1 else 4 * maker maker (x + -1) in
let times4 = fun x -> makemult makemult x in 
  times4 3

ver2: fact 5 = 4*3*2*1=24
let makemult = fun maker -> fun x ->
                 if x < 2 then 1 else (x + -1) * maker maker (x + -1) in
let fact = fun x -> makemult makemult x in 
  fact 5;;

ver3: fact 5 = 5*4*3*2*1 = 120 (complete)
let makemult = fun maker -> fun x ->
                 if x < 2 then 1 else (x + -1) * maker maker (x + -1) in
let fact = fun x -> x * makemult makemult x in 
  fact 5;;


*)