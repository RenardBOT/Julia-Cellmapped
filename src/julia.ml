open Complex
open Sys

(* ----------------- MODULE CELLULE ------------------- *)

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

(* Méthodes simplifiant la création de cellules*)
let make_cell p1 p2 =
  {p1 = p1; p2 = p2;}


(* Divise une cellule c en x parties horizontalement, et y parties verticalement.
   Par exemple subdivide c 2 2 divise une cellule en 4. 
   Retourne une liste contenant toutes les cellules, colonne par colonne*)
let subdivide cell x y =
let arr = Array.make (x*y) cell in
let dx = (cell.p2.re -. cell.p1.re) /. (float_of_int x) in
let dy = (cell.p1.im -. cell.p2.im) /. (float_of_int y) in
let rec aux i j =
  match i,j with
  | i,j when i = x -> ()
  | i,j when j = y -> aux (i+1) 0
  | i,j ->
    let p1 = {re = cell.p1.re +. (float_of_int i) *. dx; im = cell.p1.im -. (float_of_int j) *. dy} in
    let p2 = {re = cell.p1.re +. (float_of_int (i+1)) *. dx; im = cell.p1.im -. (float_of_int (j+1)) *. dy} in
    arr.(i*y+j) <- {p1 = p1; p2 = p2};
    aux i (j+1)
in aux 0 0;
arr

(* Calcule la plus petite cellule englobant le tableau des complexes passés en paramètre*) 
let bounding_box arr =
  let min_x, max_x, min_y, max_y =
    ref max_float, ref (-.max_float), ref max_float, ref (-.max_float) in
  Array.iter (fun c ->
      let re, im = c.re,c.im in
      if re < !min_x then min_x := re;
      if re > !max_x then max_x := re;
      if im < !min_y then min_y := im;
      if im > !max_y then max_y := im;
    ) arr;
  { p1 = { re = (!min_x) ; im = (!max_y)};
    p2 = {re = (!max_x) ; im = (!min_y)} }

(* Cherche si une cellule intersecte une autre ou non*)
let intersecting_cell c1 c2 =
  let max_left = max c1.p1.re c2.p1.re in
  let min_right = min c1.p2.re c2.p2.re in
  let max_bottom = max c1.p2.im c2.p2.im in
  let min_top = min c1.p1.im c2.p1.im in
  if max_left > min_right || max_bottom > min_top then false
  else true

(* Récupère les cellules d'un tableau qui intersectent une cellule c en utilisant intersecting_cell c1 c2*)
let intersecting_cells cells cell = 
  let rec aux i acc =
    match i with
    | i when i = Array.length cells -> acc
    | i when intersecting_cell cells.(i) cell -> aux (i+1) (acc @ [i]) (* REMPLACER i PAR cell pour ne pas avoir l'index mais les données*)
    | i -> aux (i+1) acc
  in aux 0 []



(* ----------------- MODULE GRAPHE ------------------- *)

(* Graphe de cellules avec une représentation via liste d'adjacence*)
type graph = { 
    vertices: cell array; 
    edges: ((int) list) array 
} 

(* Crée un graphe à partir d'un tableau de cellules *)
let make_graph cells =
  {vertices = cells; edges = Array.make (Array.length cells) []}

(* Crée un complexe à partir de deux flottants *)
let complex re im = 
  {re = re; im = im}


(* Applique une subdivision de cellules 2 2 à toutes les cellules d'un graphe *)
let subdivide_graph g =
  let arr = Array.make (Array.length g.vertices * 4) g.vertices.(0) in
  let rec aux i =
    match i with
    | i when i = Array.length g.vertices -> ()
    | i ->
      let sub = subdivide g.vertices.(i) 2 2 in
      Array.iteri (fun j x -> arr.(i*4+j) <- x) sub;
      aux (i+1)
  in aux 0;
  arr

(* Algorithme de Tarjan : Crée une liste des composantes fortement connexes du graphe*)
let tarjan graphe =
  let nb_vertices = Array.length graphe.vertices in
  let index_sommet = Array.make nb_vertices (-1) in
  let bas = Array.make nb_vertices (-1) in
  let pile = Stack.create () in
  let on_pile = Array.make nb_vertices false in
  let num = ref 0 in
  let composantes = ref [] in

  let rec dfs s =
    index_sommet.(s) <- !num;
    bas.(s) <- !num;
    num := !num + 1;
    Stack.push s pile;
    on_pile.(s) <- true;

    List.iter (fun v ->
      if index_sommet.(v) = -1 then begin
        dfs v;
        bas.(s) <- min bas.(s) bas.(v)
      end else if on_pile.(v) then
        bas.(s) <- min bas.(s) index_sommet.(v)
    ) graphe.edges.(s);

    if bas.(s) = index_sommet.(s) then begin
      let composante = ref [] in
      let rec pop_vertices () =
        let sommet = Stack.pop pile in
        on_pile.(sommet) <- false;
        composante := graphe.vertices.(sommet) :: !composante;
        if sommet <> s then pop_vertices () in
      pop_vertices ();
      composantes := !composante :: !composantes
    end in

  for i = 0 to nb_vertices - 1 do
    if index_sommet.(i) = -1 then dfs i
  done;

  !composantes


(* Renvoie la composante maximale du graphe *)
let composante_max graphe =
  let composantes = tarjan graphe in
  let cmax= List.fold_left (fun acc c -> if List.length c > List.length acc then c else acc) [] composantes in
  Array.of_list cmax


(* ----------------- MODULE JULIA ------------------- *)

(* Convertit un complex en string *)
let complex_to_string c =
    "(" ^ (string_of_float c.re) ^ " + i" ^ (string_of_float c.im) ^ ")"
    
(* Applique la fonction f(z) = z²+c *)
let julia z c =
  Complex.add (Complex.mul z z) c

(* Calcule la boite englobante de f(cellule)*)
let julia_cell cell c =
  let z1 = julia cell.p1 c in
  let z2 = julia {re = cell.p1.re; im = cell.p2.im} c in
  let z3 = julia cell.p2 c in
  let z4 = julia {re = cell.p2.re; im = cell.p1.im} c in
  let arr = [|z1;z2;z3;z4|] in
  bounding_box arr

(* Crée une liste contenant toutes les cellules d'un tableau intersectant une cellule en particulier*)
let find_intersections arr cell =
  let rec aux i acc =
    match i with
    | i when i = Array.length arr -> acc
    | i when intersecting_cell arr.(i) cell -> aux (i+1) (acc @ [i])
    | i -> aux (i+1) acc
  in aux 0 []

(* Pour chaque couple de cellule a,b, il y a une arête de a vers b ssi b intersecte f(a) *)
let build_edges g c= 
Array.iteri (fun i cell -> g.edges.(i) <- find_intersections g.vertices (julia_cell cell c)) g.vertices 



(* Algorithme appliquant les subdivisions successives pour calculer le graphe lié à un ensemble de Julia passé en paramètres, avec i itérations*)
let algo c i = 
  let total_iter = i in
  let g = make_graph [|{p1 = {re = -2.; im = 2.}; p2 = {re = 2.; im = -2.}}|] in
  let rec aux i g =
    match i with
    | i when i = 0 -> g.vertices
    | i -> 
      let start_time = Unix.gettimeofday () in
      let g' = make_graph (subdivide_graph g) in
      build_edges g' c;
      let end_time = Unix.gettimeofday () in
      let total_time = end_time -. start_time in
      Printf.printf "Execution time step %d : %f\n" (total_iter-i+1) total_time;
      aux (i-1) {vertices = composante_max g'; edges = Array.make (Array.length g'.vertices) []} in
  aux i g 



