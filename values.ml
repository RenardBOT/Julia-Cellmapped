let z1 = {re = (-2.) ; im = (2.)} ;;
let z2 = {re = (2.) ; im = (-2.)} ;;
let z3 = {re = (0.1) ; im = (0.1)} ;;
let z4 = {re = (-1.75) ; im = (1.99)} ;;
let julia_c = {re = (-0.12) ; im = (-0.77)} ;;
let c1 = { p1 = z1 ; p2 = z2 } ;;


(* Ces quatres complexes ont une bounding box de 0;1 à 1;0*)
let z5 = {re = (0.) ; im = (0.5)} ;;
let z6 = {re = (0.5) ; im = (0.)} ;;
let z7 = {re = (1.) ; im = (0.5)} ;;
let z8 = {re = (0.5) ; im = (1.)} ;;

let z9 = {re = (0.) ; im = (1.)} ;;
let z10 = {re = (1.) ; im = (0.)} ;;
let z11 = {re = (-1.) ; im = (0.)} ;;
let z12 = {re = (0.) ; im = (-1.)} ;;

let c2 = { p1 = z9 ; p2 = z10 } ;;
let c3 = { p1 = z11 ; p2 = z12 } ;;

let z13 = {re = (0.25) ; im = (0.25)} ;;
let z14 = {re = (0.75) ; im = (-.0.25)} ;;

let subdivide c x y lst = 
  let p1_x = c.p1.re in
  let p1_y = c.p1.im in
  let p2_x = c.p2.re in
  let p2_y = c.p2.im in
  let dx = (p2_x -. p1_x) /. (float_of_int x) in
  let dy = (p2_y -. p1_y) /. (float_of_int y) in
  let rec aux i j =
    match i, j with
    | i, j when i = x -> lst
    | i, j when j = y -> aux (i+1) 0
    | i, j ->
      let p1 = {re = p1_x +. (float_of_int i) *. dx; im = p1_y +. (float_of_int j) *. dy} in
      let p2 = {re = p1_x +. (float_of_int (i+1)) *. dx; im = p1_y +. (float_of_int (j+1)) *. dy} in
      aux i (j+1) @ [make_cell p1 p2]
  in aux 0 0