open Command

type direction =
  | Right
  | Left
  | Top
  | Bottom
  | Neither

type tile = {
  value : int;
  pos : int;
  neighbors : (direction * tile option) list;
}

let value { value = v; pos = p } = v
let pos { value = v; pos = p } = p

let create_tile (v : int) (p : int) (n : (direction * tile option) list) : tile
    =
  { value = v; pos = p; neighbors = n }

let find_tile tile_list pos =
  match List.filter (fun x -> x.pos = pos) tile_list with
  | [] -> None
  | h :: t -> Some h

let get_left_neighbor pos tile_list size =
  if pos mod size <> 1 then find_tile tile_list (pos - 1) else None

let get_right_neighbor pos tile_list size =
  if pos mod size <> 0 then find_tile tile_list (pos + 1) else None

let get_top_neighbor pos tile_list size =
  if pos - size >= 1 then find_tile tile_list (pos - size) else None

(**[get_bot_neighbor pos tile_list size] returns the neighbor on the bottom of
   tile with [pos] given [tile_list] and [size]. If there does not exist a
   bottom neighbor then [None] is returned.*)
let get_bot_neighbor pos tile_list size =
  if pos + size <= size * size then find_tile tile_list (pos + size) else None

(**[get_neighbors pos tile_list size] gets the neighbor on each side of the tile
   with [pos] and returns each one in a pair with it's direction relative to the
   tile. If there does not exist a neighbor in [tile_list] with the specified
   direction, the other value in the is is [None].*)
let get_neighbors pos tile_list size =
  [
    (Left, get_left_neighbor pos tile_list size);
    (Right, get_right_neighbor pos tile_list size);
    (Top, get_top_neighbor pos tile_list size);
    (Bottom, get_bot_neighbor pos tile_list size);
  ]

let get_neighbors_pertile pos tile_list size =
  match find_tile tile_list pos with
  | None -> failwith "precondition violation"
  | Some x ->
      ( x,
        [
          (Left, get_left_neighbor pos tile_list size);
          (Right, get_right_neighbor pos tile_list size);
          (Top, get_top_neighbor pos tile_list size);
          (Bottom, get_bot_neighbor pos tile_list size);
        ] )

(**[update_neighbors pos tile_list size] updates the neighbors of a tile with
   [pos] given the rest of the tiles [tile_list] and [size].*)
let update_neighbors pos tile_list size = get_neighbors pos tile_list size

(**[separate_by_column tlst size col] is a list of all the tiles in column [col]
   on the current board of [size] given by [tlist].*)
let separate_by_column (tlst : tile list) (size : int) (col : int) =
  List.filter
    (fun x ->
      (col <> size && pos x mod size = col) || (col = size && pos x mod size = 0))
    tlst

(**[separate_by_row tlst size row] is a list of all the tiles in row [row] on
   the current board of [size] given by [tlst]. *)
let separate_by_row (tlst : tile list) (size : int) (row : int) =
  List.filter (fun x -> pos x > (row - 1) * size && pos x <= row * size) tlst

(**[compare t1 t2] returns -1 if the [t1] position is less than that of [t2], 0
   if they are equal, or 1 if the [t1] position is greater than that of [t2].*)
let compare t1 t2 =
  match t1.pos - t2.pos with
  | n -> if n < 0 then -1 else if n > 0 then 1 else 0

(**[compare_rev t1 t2] returns -1 if the [t1] position is greater than that of
   [t2], 0 if they are equal and 1 if the [t1] position is less than that of
   [t2].*)
let compare_rev t1 t2 =
  match t1.pos - t2.pos with
  | n -> if n < 0 then 1 else if n > 0 then -1 else 0

(*[move_up_single col_tiles grid_tiles size upper] returns a new list of tiles
  that represents all the old tiles in the column [col_tiles], but moved up to
  their highest possible positions within the same column given all the tiles on
  the grid [grid_tiles], the size of the grid [size], and the original upper
  limit of movement [upper]. This functions requires [upper] to originally be
  set to the current column number. *)
let rec move_up_single (col_tiles : tile list) (grid_tiles : tile list)
    (size : int) (upper : int) : tile list =
  match List.sort_uniq compare col_tiles with
  | [] -> []
  | h :: t ->
      if h.pos > upper then
        let new_pos = upper in
        create_tile h.value new_pos (update_neighbors new_pos grid_tiles size)
        :: move_up_single t grid_tiles size (upper + size)
      else h :: move_up_single t grid_tiles size (upper + size)

(*[move_down_single col_tiles grid_tiles size lower] returns a new list of tiles
  that represents all the old tiles in the column [col_tiles], but moved down to
  their lowest possible positions within the same column given all the tiles on
  the grid [grid_tiles], the size of the grid [size], and the original lower
  limit of movement [lower]. This function requires [lower] to originally be set
  to the current column number + size * (size - 1). *)
let rec move_down_single (col_tiles : tile list) (grid_tiles : tile list)
    (size : int) (lower : int) : tile list =
  match List.sort_uniq compare_rev col_tiles with
  | [] -> []
  | h :: t ->
      if h.pos < lower then
        let new_pos = lower in
        create_tile h.value new_pos (update_neighbors new_pos grid_tiles size)
        :: move_down_single t grid_tiles size (lower - size)
      else h :: move_down_single t grid_tiles size (lower - size)

(*[move_right_single row_tiles grid_tiles size right] returns a new list of
  tiles that represents all the old tiles in the column of [col_tiles], but
  moved right to their rightmost positions within the same row given all the
  tiles on the grid [grid_tiles], the size of the grid [size], and the original
  rightmost limit of movement [rightmost]. This functions requires [rightmost]
  to originally be set to the current row number times the size when the
  function is called externally.*)
let rec move_right_single (row_tiles : tile list) (grid_tiles : tile list)
    (size : int) (rightmost : int) : tile list =
  match List.sort_uniq compare_rev row_tiles with
  | [] -> []
  | h :: t ->
      if h.pos < rightmost then
        let new_pos = rightmost in
        create_tile h.value new_pos (update_neighbors new_pos grid_tiles size)
        :: move_right_single t grid_tiles size (rightmost - 1)
      else h :: move_right_single t grid_tiles size (rightmost - 1)

(**[move_left_single row_tiles grid_tiles size left] returns a new list of tiles
   that represents all the old tiles in the [col_tiles], but moved left to their
   leftmost positions within the same row given [grid_tiles], the sie of the
   grid [size], and the original leftmost limit of movement [leftmost]. This
   function requires [leftmost] to originally be set to the size * (row number -
   1) \+ 1. *)
let rec move_left_single (row_tiles : tile list) (grid_tiles : tile list)
    (size : int) (leftmost : int) : tile list =
  match List.sort_uniq compare row_tiles with
  | [] -> []
  | h :: t ->
      if h.pos > leftmost then
        let new_pos = leftmost in
        create_tile h.value new_pos (update_neighbors new_pos grid_tiles size)
        :: move_left_single t grid_tiles size (leftmost + 1)
      else h :: move_left_single t grid_tiles size (leftmost + 1)

let rec move_all_up (tlst : tile list) (size : int) (col : int) =
  if col <= size then
    let col_tiles = separate_by_column tlst size col in
    move_up_single col_tiles tlst size col @ move_all_up tlst size (col + 1)
  else []

let rec move_all_down (tlst : tile list) (size : int) (col : int) =
  if col <= size then
    let col_tiles = separate_by_column tlst size col in
    move_down_single col_tiles tlst size (col + (size * (size - 1)))
    @ move_all_down tlst size (col + 1)
  else []

let rec move_all_right (tlst : tile list) (size : int) (row : int) =
  if row <= size then
    let row_tiles = separate_by_row tlst size row in
    move_right_single row_tiles tlst size (row * size)
    @ move_all_right tlst size (row + 1)
  else []

let rec move_all_left (tlst : tile list) (size : int) (row : int) =
  if row <= size then
    let row_tiles = separate_by_row tlst size row in
    move_left_single row_tiles tlst size ((size * (row - 1)) + 1)
    @ move_all_left tlst size (row + 1)
  else []

(**[move tile s] moves the tile in the direction specified by [s] by parsing [s]
   and pattern matching against the command result. If the tile is against a
   wall or another tile, then [tile] is returned. Otherwise a new tile is
   returned with the new position after it is move according to [s]. *)
let move (com : command) (tile_list : tile list) (size : int) : tile list =
  match com with
  | Up -> move_all_up tile_list size 1
  | Down -> move_all_down tile_list size 1
  | Left -> move_all_left tile_list size 1
  | Right -> move_all_right tile_list size 1
  | Start | Quit -> tile_list
