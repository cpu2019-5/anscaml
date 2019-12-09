let rec fib n =
  if n <= 1 then
    n
  else
    fib (n - 1) + fib (n - 2) in

print_int (fib 30);
print_char 10;

let rec fib2 n =
  let rec fib3 n a b =
    if n = 0 then
      b
    else
      fib3 (n - 1) (a + b) a in
  fib3 n 1 0 in
print_int (fib2 30);
print_char 10;

let rec f x = x in
let rec g x = f x * 16 in
print_char (g 3);
print_char (g 4);

let rec g2 x = f x * 16 in
print_char (g2 (read_char ()))
