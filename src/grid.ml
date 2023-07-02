open Format
open List

(* ----------------------- CREATING A GRID VALUE -------------------------*)
exception Lose
exception Full

(** (--) is a helper function that constructs a list out of the range from [i]
    to [j], indicating the columns of the grid.*)
let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux (j - 1) []

(** (---) is a helper function that constructs a list out of the range from [i]
    to [j] squared, important as the grid created is two dimensional.*)
let ( --- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux (j * j) []

type grid = {
  size : int * int;
  pos_list : int list;
  tiles : Tile.tile list;
}

let create_grid (dim : int) : grid =
  { size = (dim, dim); pos_list = 1 --- dim; tiles = [] }

(*[filled_list grid] returns a list of filled positions. If there are none, it
  returns an empty list.*)
let rec filled_list grid =
  List.fold_left (fun acc x -> Tile.pos x :: acc) [] grid.tiles

(*[empty_list grid] returns a list of empty positions. If there are none, it
  returns an empty list*)
let empty_list grid =
  let filled = filled_list grid in
  List.filter (fun x -> List.mem x filled = false) grid.pos_list

(** [random_square] gives a random position in the grid which will be filled in
    by a new tile. It is a helper function to [populate_grid].*)
let rec random_square (grid : grid) : int =
  Random.self_init ();
  let empty = empty_list grid in
  if List.length empty > 0 then
    let index = Random.int (List.length empty) in
    if List.nth empty index = 1 then 2 else List.nth empty index
  else raise Full

let grid_size (grid : grid) : int = fst grid.size
let grid_tiles (grid : grid) : Tile.tile list = grid.tiles

let populate_grid (grid : grid) : grid =
  {
    size = grid.size;
    pos_list = grid.pos_list;
    tiles = Tile.create_tile 2 (random_square grid) [] :: grid.tiles;
  }

(*[random_gen_tile] generates a number either 2 or 4 to be assigned to a new
  tile*)
let random_gen_tile () : int =
  Random.self_init ();
  Random.int 21 |> fun x -> if x <= 14 then 2 else 4

(*takes in a grid and tile lst and returns the same grid with the updates
  tiles*)
let set_tiles grid lst = { grid with tiles = lst }

(*[add_tile grid] generates a new tile and adds it to an available position from
  empty_list grid. This tile is added to grid.tiles. If the empty_list grid is
  empty returns input grid.*)
let add_tile (grid : grid) lst : Tile.tile list =
  let new_tile =
    Tile.create_tile (random_gen_tile ()) (random_square grid) []
  in
  new_tile :: lst

(**[remove_tile grid t] returns grid with tile t removed from grid.tiles*)
let rec remove_tile grid l : grid =
  {
    grid with
    tiles = List.filter (fun x -> Tile.pos x != Tile.pos l) grid.tiles;
  }

let set_tiles grid lst = { grid with tiles = lst }

(** Prints the top of the small tile.*)
let grid_top () : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] ".______."

(** Prints the middle of the small tile.*)
let grid_middle () : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] "|      |"

(** Prints the value of the small tile denoted by string [n].*)
let grid_value (n : string) : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] ("|  " ^ n ^ "   |")

(** Prints the value of the small tile denoted by string [n].*)
let grid_value2 (n : string) : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] ("|  " ^ n ^ "  |")

let grid_value3 (n : string) : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] ("| " ^ n ^ "  |")

let grid_value4 (n : string) : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] ("| " ^ n ^ " |")

(** Prints the bottom of the small tile.*)
let grid_bottom () : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] "|______|"

let keys boxes = List.map (fun x -> fst x) boxes

(* Prints the first line of a single row of a large grid.*)
let rec first_col (num : int) : unit =
  print_string "____________";
  if num > 1 then first_col (num - 1) else print_string "_\n"

(* Prints the second line of a single row of a large grid.*)
let rec second_col (size : int) (num : int) (boxes : (int * int) list) : unit =
  if List.mem (size - num) (keys boxes) then (
    print_string " | ";
    grid_top ();
    print_string " ";
    if num >= 0 then second_col size (num - 1) boxes else print_string " |\n")
  else if num >= 0 then (
    print_string " |          ";
    second_col size (num - 1) boxes)
  else print_string " |\n"

(* Prints the third line of a single row of a large grid.*)
let rec third_col (size : int) (num : int) (boxes : (int * int) list) : unit =
  if List.mem (size - num) (keys boxes) then (
    print_string " | ";
    grid_middle ();
    print_string " ";
    if num >= 0 then third_col size (num - 1) boxes else print_string " |\n")
  else if num >= 0 then (
    print_string " |          ";
    third_col size (num - 1) boxes)
  else print_string " |\n"

let rec find_val (col : int) (boxes : (int * int) list) : int =
  match boxes with
  | [] -> 0
  | (c, v) :: t -> if c = col then v else find_val col t

(* Prints the fourth line of a single row of a large grid.*)
let rec fourth_col (size : int) (num : int) (boxes : (int * int) list) : unit =
  let v = find_val (size - num) boxes in
  let grid_val str =
    if v > 0 then
      if v / 10 > 0 then
        if v / 100 > 0 then
          if v / 1000 > 0 then grid_value4 (string_of_int v)
          else grid_value3 (string_of_int v)
        else grid_value2 (string_of_int v)
      else grid_value (string_of_int v)
    else grid_middle ()
  in
  if List.mem (size - num) (keys boxes) then (
    print_string " | ";
    grid_val v;
    print_string " ";
    if num >= 0 then fourth_col size (num - 1) boxes else print_string " |\n")
  else if num >= 0 then (
    print_string " |          ";
    fourth_col size (num - 1) boxes)
  else print_string " |\n"

(* Prints the fifth line of a single row of a large grid.*)
let rec fifth_col (size : int) (num : int) (boxes : (int * int) list) : unit =
  if List.mem (size - num) (keys boxes) then (
    print_string " | ";
    grid_bottom ();
    print_string " ";
    if num >= 0 then fifth_col size (num - 1) boxes else print_string " |\n")
  else if num >= 0 then (
    print_string " |          ";
    fifth_col size (num - 1) boxes)
  else print_string " |\n"

(* Prints the sixth line of a single row of a large grid.*)
let rec sixth_col (num : int) : unit =
  print_string "|___________";
  if num > 1 then sixth_col (num - 1) else print_string "|\n"

let rec row_box (boxes : (int * int * int) list) (row : int)
    (acc : (int * int) list) =
  match boxes with
  | [] -> acc
  | (r, c, v) :: t ->
      if r = row then row_box t row ((c, v) :: acc) else row_box t row acc

let top_row (size : int) (boxes : (int * int * int) list) : unit =
  let row1 = row_box boxes 1 [] in
  print_string " ";
  first_col size;
  second_col size (size - 1) row1;
  third_col size (size - 1) row1;
  fourth_col size (size - 1) row1;
  fifth_col size (size - 1) row1;
  print_string " ";
  sixth_col size

let rec rest_of_grid (size : int) (boxes : (int * int * int) list) (rn : int) =
  if rn <= size then (
    let row_num = row_box boxes rn [] in
    second_col size (size - 1) row_num;
    third_col size (size - 1) row_num;
    fourth_col size (size - 1) row_num;
    fifth_col size (size - 1) row_num;
    print_string " ";
    sixth_col size;
    rest_of_grid size boxes (rn + 1))
  else ()

let show_values (grd : grid) : (int * int) list =
  List.map (fun x -> (Tile.pos x, Tile.value x)) (grid_tiles grd)

let print_all_boxes (grd : grid) (size : int) : unit =
  let filled_boxes =
    List.map2
      (fun x y ->
        ( (if x mod size = 0 then x / size else (x / size) + 1),
          (if x mod size = 0 then 5 else x mod size),
          snd y ))
      (filled_list grd)
      (List.rev (show_values grd))
  in
  top_row size filled_boxes;
  rest_of_grid size filled_boxes 2
