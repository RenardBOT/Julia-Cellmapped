(* array containing various squares defined by a corner and their side length*)
type workspace = {
  min_x : float;
  min_y : float;
  max_x : float;
  max_y : float;
}

let julia_workspace = {
  min_x = -.2.;
  min_y = -.2.;
  max_x = 2.;
  max_y = 2.;
}

let draw cells workspace =
  let width = 500 in
  let height = 500 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:height in
  let cr = Cairo.create surface in
  Cairo.scale cr (float_of_int width) (float_of_int height) ;

  let draw_cell cr workspace cell =
    Cairo.set_source_rgb cr 1. 1. 1.;
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

  Cairo.PNG.write surface "julia.png"
