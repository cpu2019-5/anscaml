let rec stdlib_print_int_pos i =
  if i <> 0 then (
    stdlib_print_int_pos (i / 10);
    print_char (48 + (i % 10))
  ) else () in

let rec print_int i =
  if i = 0 then
    print_char 48 (* '0' *)
  else if i = -2147483648 then (* cannot calc abs properly *)
    print_char 45; print_char 50; print_char 49; print_char 52; print_char 55; print_char 52;
    print_char 56; print_char 51; print_char 54; print_char 52; print_char 56;
  else
    let abs =
      if i < 0 then (print_char 45; 0 - i) (* '-' *)
      else i in
    stdlib_print_int_pos abs in

let rec fequal x y = x =. y in
let rec fless x y = x <. y in
let rec fispos x = x >. 0.0 in
let rec fisneg x = x <. 0.0 in
let rec fiszero x = x =. 0.0 in
let rec xor a b = a <> b in

(* http://takashiijiri.com/study/miscs/fastsqrt.html *)
let rec sqrt x =
  let x_half = x *. 0.5 in
  let r = float_of_bits (1597463007 - (bits_of_float x) / 2) in
  let r = r *. (1.5 -. x_half *. r *. r) in
  let r = r *. (1.5 -. x_half *. r *. r) in
  let r = r *. (1.5 -. x_half *. r *. r) in
  r *. x in

(*
  三角関数(stdlib_kcos,stdlib_ksin,cos,sin,atan): fdlibm, msunを移植
  http://www.netlib.org/fdlibm/k_cos.c
  http://www.netlib.org/fdlibm/k_sin.c
  http://www.netlib.org/fdlibm/s_cos.c
  http://www.netlib.org/fdlibm/s_sin.c
  https://github.com/freebsd/freebsd/blob/a69d6f3a/lib/msun/src/s_atanf.c
  ====================================================
  Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
  Developed at SunSoft, a Sun Microsystems, Inc. business.
  Permission to use, copy, modify, and distribute this
  software is freely granted, provided that this notice
  is preserved.
  ====================================================
*)
let rec stdlib_kcos x y =
  if x <. 0.0 then stdlib_kcos (0.0 -. x) (0.0 -. y) else
  if x <. 0.000000007450580596923828 (* 2^-27 *) then 1.0 else
  let c1 =  0.0416666666666666019037 in
  let c2 = -0.00138888888888741095749 in
  let c3 =  0.0000248015872894767294178 in
  let c4 = -0.000000275573143513906633035 in
  let c5 =  0.00000000208757232129817482790 in
  let c6 = -0.0000000000113596475577881948265 in
  let z = x *. x in
  let r = z *. (c1 +. z *. (c2 +. z *. (c3 +. z *. (c4 +. z *. (c5 +. z *. c6))))) in
  let qx = if x >. 0.78125 then 0.28125 else x /. 4.0 in
  let hz = 0.5 *. z -. qx in
  let a = 1.0 -. qx in
  a -. (hz -. (z *. r -. x *. y)) in
let rec stdlib_ksin x y iy0 =
  if x <. 0.0 then 0.0 -. stdlib_ksin (0.0 -. x) (0.0 -. y) iy0 else
  if x <. 0.000000007450580596923828 (* 2^-27 *) then x else
  let s1 = -0.166666666666666324348 in
  let s2 =  0.00833333333332248946124 in
  let s3 = -0.000198412698298579493134 in
  let s4 =  0.00000275573137070700676789 in
  let s5 = -0.0000000250507602534068634195 in
  let s6 =  0.000000000158969099521155010221 in
  let z = x *. x in
  let v = z *. x in
  let r = s2 +. z *. (s3 +. z *. (s4 +. z *. (s5 +. z *. s6))) in
  if iy0 then x +. v *. (s1 +. z *. r)
  else       x -. ((z *. (0.5 *. y -. v *. r) -. y) -. v *. s1) in
let rec cos [@no_inline] x =
  let pi = 3.141592653589793238462643383279 in
  let pi2 = pi *. 2.0 in
  let piq = pi *. 0.25 in
  let x = fabs x in
  if x <. piq then stdlib_kcos x 0.0 else
  let y = x -. (floor (x /. pi2)) *. pi2 in
  if y <. piq then
    stdlib_kcos y 0.0 (* TODO: y[1] *)
  else if y <. piq *. 3.0 then
    0.0 -. stdlib_ksin (y -. piq *. 2.0) 0.0 false
  else if y <. piq *. 5.0 then
    0.0 -. stdlib_kcos (y -. piq *. 4.0) 0.0
  else if y <. piq *. 7.0 then
    stdlib_ksin (y -. piq *. 6.0) 0.0 false
  else
    stdlib_kcos (y -. piq *. 8.0) 0.0 in
let rec sin [@no_inline] x =
  let pi = 3.141592653589793238462643383279 in
  let pi2 = pi *. 2.0 in
  let piq = pi *. 0.25 in
  if x <. 0.0 then 0.0 -. sin (0.0 -. x) else
  if x <. piq then stdlib_ksin x 0.0 true else
  let y = x -. (floor (x /. pi2)) *. pi2 in
  if y <. piq then
    stdlib_ksin y 0.0 false
  else if y <. piq *. 3.0 then
    stdlib_kcos (y -. piq *. 2.0) 0.0
  else if y <. piq *. 5.0 then
    0.0 -. stdlib_ksin (y -. piq *. 4.0) 0.0 false
  else if y <. piq *. 7.0 then
    0.0 -. stdlib_kcos (y -. piq *. 6.0) 0.0
  else
    stdlib_ksin (y -. piq *. 8.0) 0.0 false in
let rec atan x =
  let hi0 = 0.46364760399 in
  let hi1 = 0.78539812565 in
  let hi2 = 0.98279368877 in
  let hi3 = 1.5707962513 in
  let lo0 = 0.0000000050121582440 in
  let lo1 = 0.000000037748947079 in
  let lo2 = 0.000000034473217170 in
  let lo3 = 0.000000075497894159 in
  let at0 =  0.33333328366 in
  let at1 = -0.19999158382 in
  let at2 =  0.14253635705 in
  let at3 = -0.10648017377 in
  let at4 =  0.061687607318 in
  if x <. 0.0 then 0.0 -. atan (0.0 -. x) else
  if x >=. 67108864.0 then hi3 +. lo3 else
  if x <. 0.000244140625 (* 2**-12 *) then x else
  let (idneg, hi, lo, x) =
    if x <. 0.4375 (* 7/16 *) then
      (true, 0.0, 0.0, x)
    else if x <=. 0.6875 (* 11/16 *) then
      (false, hi0, lo0, (2.0 *. x -. 1.0) /. (2.0 +. x))
    else if x <=. 1.1875 (* 19/16 *) then
      (false, hi1, lo1, (x -. 1.0) /. (x +. 1.0))
    else if x <=. 2.4375 then
      (false, hi2, lo2, (x -. 1.5) /. (1.0 +. 1.5 *. x))
    else
      (false, hi3, lo3, -1.0 /. x) in
  let z = x *. x in
  let w = z *. z in
  let s1 = z *. (at0 +. w *. (at2 +. w *. at4)) in
	let s2 = w *. (at1 +. w *. at3) in
  if idneg then
    x -. x *. (s1 +. s2)
  else
    hi -. ((x *. (s1 +. s2) -. lo) -. x) in

let rec stdlib_int_of_float_rec [@no_inline] f a b =
  if b - a = 1
  then a
  else
    let m = a / 2 + b / 2 + (a % 2 + b % 2) / 2 in
    if float_of_int m >. f then
      stdlib_int_of_float_rec f a m
    else
      stdlib_int_of_float_rec f m b in

let rec stdlib_int_of_float_pos f =
  if f <. 8388608.0 then
    bits_of_float (f +. 8388608.0) - 1258291200 (* 神資料 *)
  else
    (* if f >=. 2147483646.5 then 2147483647 else (* レギュの定義域では絶対false *) *)
    stdlib_int_of_float_rec (f +. 0.5) 0 2147483647 in

let rec int_of_float f =
  if f <. 0.0 then
    if f =. -2147483648.0 then -2147483648
    else 0 - (stdlib_int_of_float_pos (0.0 -. f))
  else
    stdlib_int_of_float_pos f in

let rec read_int _ =
  ((read_char () * 256 + read_char ()) * 256 + read_char ()) * 256 + read_char () in

let rec read_float _ =
  float_of_bits (read_int ()) in

()
