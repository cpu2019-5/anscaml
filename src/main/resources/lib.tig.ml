let rec stdlib_print_int_pos [@typ_fold] i =
  if i <> 0 then (
    let j = i / 10 in
    stdlib_print_int_pos j;
    print_char (48 + i - j * 8 - j * 2)
  ) else () in

let rec print_int [@typ_fold] i =
  if i = 0 then
    print_char 48 (* '0' *)
  else if i = (-1) - 2147483647 then (* cannot calc abs properly *)
    print_char 45; print_char 50; print_char 49; print_char 52; print_char 55; print_char 52;
    print_char 56; print_char 51; print_char 54; print_char 52; print_char 56;
  else
    let abs =
      if i < 0 then (print_char 45; (-i)) (* '-' *)
      else i in
    stdlib_print_int_pos abs in

let rec fequal x y = x =. y in
let rec fless x y = x <. y in
let rec fispos x = x >. 0.0 in
let rec fisneg x = x <. 0.0 in
let rec fiszero x = x =. 0.0 in
let rec xor a b = if a then not b else b in

let rec fhalf x = x *. 0.5 in
let rec fsqr x = x *. x in

(* http://takashiijiri.com/study/miscs/fastsqrt.html *)
(*let rec sqrt x =
  if x <. 0.0 then 0.0 else
  let x_half = x *. 0.5 in
  let r = float_of_bits (1597463007 - (bits_of_float x) / 2) in
  let r = r *. (1.5 -. x_half *. r *. r) in
  let r = r *. (1.5 -. x_half *. r *. r) in
  let r = r *. (1.5 -. x_half *. r *. r) in
  r *. x in*)

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
let stdlib__pi2 = 3.141592653589793238462643383279 *. 2.0 in
let rec stdlib__mod_2pi_loop1 a p =
  if a <. p then p
  else stdlib__mod_2pi_loop1 a (p *. 2.0) in
let rec stdlib__mod_2pi_loop2 a p pi2 =
  if a <. pi2 then a
  else stdlib__mod_2pi_loop2 (if a >=. p then a -. p else a) (p *. 0.5) pi2 in
let rec stdlib__mod_2pi a =
  let pi2 = stdlib__pi2 in
  if a <. pi2 then a
  else stdlib__mod_2pi_loop2 a (stdlib__mod_2pi_loop1 a pi2) pi2 in
let rec stdlib__kcos xx yy =
  let x = fabs xx in
  if x <. 0.000000007450580596923828 (* 2^-27 *) then 1.0 else
  let y = if x <. 0.0 then 0.0 -. yy else yy in
  let c1 =  0.0416666666666666019037 in
  let c2 = -0.00138888888888741095749 in
  let c3 =  0.0000248015872894767294178 in
  let c4 = -0.000000275573143513906633035 in
  let c5 =  0.00000000208757232129817482790 in
  let c6 = -0.0000000000113596475577881948265 in
  let z = x *. x in
  let r = z *. (c1 +. z *. (c2 +. z *. (c3 +. z *. (c4 +. z *. (c5 +. z *. c6))))) in
  let qx = if x >. 0.78125 then 0.28125 else x *. 0.25 in
  let hz = 0.5 *. z -. qx in
  let a = 1.0 -. qx in
  a -. (hz -. (z *. r -. x *. y)) in
let rec stdlib__ksin_pos x y iy0 =
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
let rec stdlib__ksin x y iy0 =
  if x <. 0.0 then 0.0 -. (stdlib__ksin_pos (0.0 -. x) (0.0 -. y) iy0) else
  stdlib__ksin_pos x y iy0 in
let rec cos [@no_inline] x =
  let pi = 3.141592653589793238462643383279 in
  let piq = pi *. 0.25 in
  let x = fabs x in
  if x <. piq then stdlib__kcos x 0.0 else
  let y = stdlib__mod_2pi x in
  if y <. piq then
    stdlib__kcos y 0.0 (* TODO: y[1] *)
  else if y <. piq *. 3.0 then
    0.0 -. (stdlib__ksin (y -. piq *. 2.0) 0.0 false)
  else if y <. piq *. 5.0 then
    0.0 -. (stdlib__kcos (y -. piq *. 4.0) 0.0)
  else if y <. piq *. 7.0 then
    stdlib__ksin (y -. piq *. 6.0) 0.0 false
  else
    stdlib__kcos (y -. piq *. 8.0) 0.0 in
let rec stdlib_sin_pos x =
  let pi = 3.141592653589793238462643383279 in
  let piq = pi *. 0.25 in
  if x <. piq then stdlib__ksin x 0.0 true else
  let y = stdlib__mod_2pi x in
  if y <. piq then
    stdlib__ksin y 0.0 false
  else if y <. piq *. 3.0 then
    stdlib__kcos (y -. piq *. 2.0) 0.0
  else if y <. piq *. 5.0 then
    0.0 -. (stdlib__ksin (y -. piq *. 4.0) 0.0 false)
  else if y <. piq *. 7.0 then
    0.0 -. (stdlib__kcos (y -. piq *. 6.0) 0.0)
  else
    stdlib__ksin (y -. piq *. 8.0) 0.0 false in
let rec sin [@no_inline] x =
  if x <. 0.0 then 0.0 -. stdlib_sin_pos (0.0 -. x)
  else stdlib_sin_pos x in

let rec stdlib_atan_base x =
  let x2 = x *. x in
  x *. (1.0 -. x2 *. (0.3333333 -. x2 *. (0.2 -. x2 *. (
    0.142857142 -. x2 *. (0.111111104 -. x2 *. (0.08976446 -. x2 *. 0.060035485)))))) in
let rec atan [@no_inline] x =
  let pi = 3.141592653589793238462643383279 in
  if x <. 0.0 then
    pi *. 0.5 +. stdlib_atan_base (0.0 -. x)
  else if x <. 0.4375 then
    stdlib_atan_base x
  else if x <. 2.4375 then
    pi *. 0.25 +. stdlib_atan_base ((x -. 1.0) /. (x +. 1.0))
  else
    pi *. 0.5 -. stdlib_atan_base (1.0 /. x) in

let rec read_int _ =
  ((read_char () * 256 + read_char ()) * 256 + read_char ()) * 256 + read_char () in

let rec read_float _ =
  float_of_bits (read_int ()) in

()
