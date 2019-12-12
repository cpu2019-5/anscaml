let a = create_array 3 (read_char ()) in
let rec f b =
  print_int a.(0); print_char 32;
  print_int a.(b); print_char 10 in
f 2;
f 3;
f 4;
f 5;
f 6;
print_int a.(3)
