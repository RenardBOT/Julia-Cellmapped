let draw_julia cre cim i hsv_h hsv_l filename =
  let start_time = Unix.gettimeofday () in
  let cells = algo (complex cre cim) i in
  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  draw cells julia_workspace hsv_h hsv_l filename;
  Printf.printf "Execution time: %.6f seconds\n" execution_time;
  Printf.printf "Amount of cells: %d" (Array.length cells)