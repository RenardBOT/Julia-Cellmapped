open Complex

(* TYPES *)


(*  Une cellule est représentée par p1 son coin supérieur gauche, et p2 son coin inférieur droit :

       p1 +-------+
          |       |
          |       |
          |       |
          +-------+ p2 

    p1 et p2 sont des complexes, et p1.re < p2.re et p1.im > p2.im.
    rappellons qu'un complexe dans le plan |C est semblable à une coordonnée (x,y) dans le plan |R², avec x = re et y = im. 
*)
type cell = { 
    p1 : Complex.t;
    p2 : Complex.t; 
} 

(* Graphe de cellules avec une représentation via liste d'adjacence*)
type graph = { 
    vertices: cell array; 
    edges: ((int) list) array 
} 

(* tosrtrings pour debug *)

let complex_to_string z =
  Printf.sprintf "%s + i%s" (Float.to_string z.re) (Float.to_string z.im)

let cell_to_string c =
  Printf.sprintf "Cell( %s ; %s )" (complex_to_string c.p1) (complex_to_string c.p2)

(* Méthodes simplifiant la création de cellules et de complexes*)

let make_cell p1 p2 =
  {p1 = p1; p2 = p2;}

let complex re im = 
  {re = re; im = im}

(* Divise une cellule c en x parties horizontalement, et y parties verticalement.
   Par exemple subdivide c 2 2 divise une cellule en 4. 
   Retourne une liste contenant toutes les cellules, colonne par colonne*)
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

(* Affiche les sommets du graphe *)
let print_vertices g =
  Array.iter (fun x -> Printf.printf "%s\n" (cell_to_string x)) g.vertices

(* Applique la fonction f(z) = z²+c *)
let julia z c =
  Complex.add (Complex.mul z z) c

(* Calcule la plus petite cellule englobant le tableau des complexes passés en paramètre*) 
let bounding_box arr =
  let min_x, max_x, min_y, max_y =
    ref max_float, ref min_float, ref max_float, ref min_float in
  Array.iter (fun c ->
      let re, im = c.re,c.im in
      if re < !min_x then min_x := re;
      if re > !max_x then max_x := re;
      if im < !min_y then min_y := im;
      if im > !max_y then max_y := im;
    ) arr;
  { p1 = { re = (!min_x) ; im = (!max_y)};
    p2 = {re = (!max_x) ; im = (!min_y)} };

(* Cherche si une cellule intersecte une autre ou non*)
let intersecting_cell c1 c2 =
  let max_left = max c1.p1.re c2.p1.re in
  let min_right = min c1.p2.re c2.p2.re in
  let max_bottom = max c1.p2.im c2.p2.im in
  let min_top = min c1.p1.im c2.p1.im in
  if max_left > min_right || max_bottom > min_top then false
  else true

(* Récupère les cellules d'un tableau qui intersectent une cellule c en utilisant intersecting_cell c1 c2*)
let intersecting_cells cells c = 
  let rec aux i acc =
    match i with
    | i when i = Array.length cells -> acc
    | i when intersecting_cell cells.(i) c -> aux (i+1) (acc @ [i]) (* REMPLACER i PAR c pour ne pas avoir l'index mais les données*)
    | i -> aux (i+1) acc
  in aux 0 []

  (* Il reste à : 
     - Calculer une approximation de f(cellule) avec l'arithmétique des intervalles (trouver une boîte englobante de f(cellule) quoi)
     - Créer une arête entre une cellule c et toutes les cellules comprises dans la boîte englobante de f(c) dans le graphe
     - Retrouver toutes les composantes fortement connexes de taille >2 
     - Subdiviser toutes ces cellules faisant partie de ces composantes et créer un nouveau graphe avec 
     - Représenter graphiquement * ) 