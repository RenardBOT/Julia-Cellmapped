open Complex
open Stack

type cell = {
      p1 : Complex.t;
      p2 : Complex.t;
}

(* Graphes de test *)

(* ABC forme une composante fortement connexe, DE aussi *)
let g1 = {
  vertices = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H' |];
  edges = [| [1]; [2]; [0; 3]; [4]; [3]; [5; 7]; [4]; [6] |]
}

(* v3 v4 v5 forment une composante fortement connexe, voir papier du sujet figure 2*)

let g2 = {
  vertices = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7' ; '8'|];
  edges = [| [3;4]; [3;4;5]; [3;4]; [3;4;5]; [4;5]; [5;3;4]; [3;4;5]; [5] ; [3;4;5] |]
}

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
;;

let composante_max graphe =
  let composantes = tarjan graphe in
  let cmax= List.fold_left (fun acc c -> if List.length c > List.length acc then c else acc) [] composantes in
  Array.of_list cmax
