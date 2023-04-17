open Cairo

(* TYPES *)

(* Définit les coordonnées min et max (dans la hauteur et la largeur) d'un espace quelconque à afficher *)
type workspace = {
  min_x : float;
  min_y : float;
  max_x : float;
  max_y : float;
}

(* Définit l'espace utilisé pour afficher l'ensemble de Julia*)
let julia_workspace = {
  min_x = -.2.;
  min_y = -.2.;
  max_x = 2.;
  max_y = 2.;
}

type hsv_color = { h : float; s : float; v : float }
type rgb_color = { r : float; g : float; b : float }

(* Convertit une couleur du format teinte-saturation-luminosité vers rouge-vert-bleu*)
let hsv_to_rgb (hsv : hsv_color) : rgb_color =
  let c = hsv.v *. hsv.s in
  let h' = hsv.h /. 60.0 in
  let x = c *. (1.0 -. abs_float (mod_float h' 2.0 -. 1.0)) in
  let (r', g', b') =
    if h' < 1.0 then (c, x, 0.0)
    else if h' < 2.0 then (x, c, 0.0)
    else if h' < 3.0 then (0.0, c, x)
    else if h' < 4.0 then (0.0, x, c)
    else if h' < 5.0 then (x, 0.0, c)
    else (c, 0.0, x)
  in
  let m = hsv.v -. c in
  { r = r' +. m; g = g' +. m; b = b' +. m }

(* Dessine chaque cellule dans une image *)
let draw cells workspace hsv_h hsv_l filename =
  let width = 2000 in
  let height = 2000 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:height in
  let cr = Cairo.create surface in
  Cairo.scale cr (float_of_int width) (float_of_int height) ;

  let draw_cell cr workspace cell =
    let hsv_color = { h = hsv_h; s = 1.; v = hsv_l } in
    let rgb_color = hsv_to_rgb hsv_color in
    Cairo.set_source_rgb cr rgb_color.r rgb_color.g rgb_color.b;
    let x = cell.p1.re in
    let y = cell.p1.im in
    let side = cell.p2.re -. cell.p1.re in
    let x = (x -. workspace.min_x) /. (workspace.max_x -. workspace.min_x) in
    let y = 1. -. (y -. workspace.min_y) /. (workspace.max_y -. workspace.min_y) in
    let side = side /. (workspace.max_x -. workspace.min_x) in
    Cairo.rectangle cr x y ~w:side ~h:side;
    Cairo.fill cr in

  let draw_cells cells cr workspace =
    print_endline "Drawing cells";
    Array.iter (draw_cell cr workspace) cells in

  draw_cells cells cr workspace;

  Cairo.PNG.write surface (filename^".png")

