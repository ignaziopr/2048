type t = {
  grid : Grid.grid;
  score : int;
}

(* [direction_list] is an association list that contains tuples with a command
   as its first element and a direction as its second element.*)
let direction_list : (Command.command * Tile.direction) list =
  [ (Up, Top); (Down, Bottom); (Left, Left); (Right, Right) ]

(** (---) is a helper function that constructs a list out of the range from [i]
    to [j] squared, important as the grid created is two dimensional.*)
let ( --- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux (j * j) []

(** [sort_tiles] is a tile list produced by taking the [grid_tiles] field from
    the [grid] provided and sorts it based on the direction [com] given and the
    position of each tile. For example, if the given direction is Up and the
    tile list consists of tiles with positions [1;7;3;19;14;22], the tile_list
    will be sorted in the following order of tiles: tiles with position
    [1;3;7;14;19;22], with the tiles at the top being first in the list so that
    they are the first ones to be moved up. Keep in mind that tiles are in the
    list, and not positions of the tiles. However, the tiles are sorted by
    position.*)
let sort_tiles grid (d : Tile.direction) =
  let tiles = Grid.grid_tiles grid in
  match d with
  | Top ->
      List.sort (fun x y -> if Tile.pos x > Tile.pos y then 1 else -1) tiles
  | Bottom ->
      List.sort (fun x y -> if Tile.pos x > Tile.pos y then -1 else 1) tiles
  | Left ->
      List.sort
        (fun x y ->
          if
            Tile.pos x mod Grid.grid_size grid
            > Tile.pos y mod Grid.grid_size grid
          then 1
          else -1)
        tiles
  | Right ->
      List.sort
        (fun x y ->
          if
            Tile.pos x mod Grid.grid_size grid
            > Tile.pos y mod Grid.grid_size grid
          then -1
          else 1)
        tiles
  | Neither -> failwith "Should not be used"

(** [move_grid com] takes in grid and moves all the tiles in the grid according
    to a command *)
let move_grid (com : Command.command) grid =
  match com with
  | Up -> Tile.move Up (Grid.grid_tiles grid) (Grid.grid_size grid)
  | Down -> Tile.move Down (Grid.grid_tiles grid) (Grid.grid_size grid)
  | Left -> Tile.move Left (Grid.grid_tiles grid) (Grid.grid_size grid)
  | Right -> Tile.move Right (Grid.grid_tiles grid) (Grid.grid_size grid)
  | Start | Quit -> failwith "unimplemented"

(** [tiles_to_match grid] produces a list made up of a pair of elements. The
    first element is a tile, while the second element is a list of the first
    tile's neighbors. Each neighbor list contains a tuple consisting of a
    direction and a tile option. Essentially, [tiles_to_match] shows every tile
    and their neighbors in every direction.*)
let tiles_to_match (grid : Grid.grid) (d : Tile.direction) =
  let filled = Grid.filled_list grid in
  let tiles = sort_tiles grid d in
  List.map
    (fun x -> Tile.get_neighbors_pertile x tiles (Grid.grid_size grid))
    filled

(** [sort_closer tt d] takes in a pair of tiles [tt] and indicates which tile is
    closest to the grid border relative to direction [d], reorganzing the pair
    and making the closest tile the first element and the farthest tile the
    second element. For example, if [tt] consists of a tile with position 11 and
    a tile with position 14, and the direction [d] given is Left, then the new
    pair would be (Tile 11, Tile 14). *)
let sort_closer (tt : (Tile.tile * Tile.tile) list) (d : Tile.direction)
    (size : int) =
  let rec compare pair (d : Tile.direction) size =
    match d with
    | Top ->
        if Tile.pos (fst pair) < Tile.pos (snd pair) then pair
        else (snd pair, fst pair)
    | Bottom ->
        if Tile.pos (fst pair) > Tile.pos (snd pair) then pair
        else (snd pair, fst pair)
    | Left ->
        let l_box =
          if Tile.pos (fst pair) mod size = 0 then 5
          else Tile.pos (fst pair) mod size
        in
        let r_box =
          if Tile.pos (snd pair) mod size = 0 then 5
          else Tile.pos (snd pair) mod size
        in
        if l_box < r_box then (fst pair, snd pair) else (snd pair, fst pair)
    | Right ->
        let l_box =
          if Tile.pos (fst pair) mod size = 0 then 5
          else Tile.pos (fst pair) mod size
        in
        let r_box =
          if Tile.pos (snd pair) mod size = 0 then 5
          else Tile.pos (snd pair) mod size
        in
        if l_box > r_box then (fst pair, snd pair) else (snd pair, fst pair)
    | _ -> failwith "no"
  in
  List.map (fun x -> compare x d size) tt

(** [closer grid d] takes in a grid [grid] and a direction [d] and returns a
    list of tile pairings in which each tile has a direct neighbor, organized by
    which tile is closest to the grid border and the direction passed in.*)
let closer (grid : Grid.grid)
    (d : Tile.direction) (*: (Tile.tile * Tile.tile) list *) =
  let match_dir (pair : Tile.direction * Tile.tile option) (di : Tile.direction)
      =
    match (pair, di) with
    | (Left, Some ti), Left
    | (Right, Some ti), Right
    | (Top, Some ti), Top
    | (Bottom, Some ti), Bottom -> true
    | _ -> false
  in
  let tiles = tiles_to_match grid d in
  let filter_dir h = List.filter (fun y -> match_dir y d) (snd h) in
  let one_entry h d =
    if List.length (filter_dir h) > 0 then snd (List.nth (filter_dir h) 0)
    else None
  in
  let rec reduce_by_dir lst acc =
    match lst with
    | [] -> acc
    | h :: t -> reduce_by_dir t ((fst h, one_entry h d) :: acc)
  in
  let pair2 = reduce_by_dir tiles [] in
  let pairing pa =
    match pa with
    | None -> false
    | Some x -> true
  in
  let no_nones = List.filter (fun (a, z) -> pairing z) pair2 in
  sort_closer
    (List.map
       (fun x ->
         ( fst x,
           match snd x with
           | Some y -> y
           | _ -> failwith "never" ))
       no_nones)
    d (Grid.grid_size grid)

(** [removed_dups pairing] takes in a list of all the tile pairings, which
    include a tile and its neighbor, and remove any duplicate tile entries. For
    example, if the move direction was Left and [pairings] consists of
    [(Tile in position 11, Tile in position 12);
      (Tile in position 12, Tile in position 13);
      (Tile in position 13, Tile in position 14)]
    then the middle pair will be deleted, producing the tile-neighbor list
    [(Tile in position 11, Tile in position 12);
      (Tile in position 13, Tile in position 14)].
    This way, a tile can only combine with one other tile. *)
let remove_dups (pairings : (Tile.tile * Tile.tile) list) =
  let keys = fst (List.split pairings) in
  let values = snd (List.split pairings) in
  let rec removed_copies pairings keys values acc klist vlist =
    match (pairings, keys, values) with
    | h :: t, h2 :: t2, h3 :: t3 ->
        if
          List.mem (fst h) t2 = false
          && List.mem (snd h) t3 = false
          && List.mem (fst h) vlist = false
          && Tile.value (fst h) = Tile.value (snd h)
        then removed_copies t t2 t3 (h :: acc) klist vlist
        else
          removed_copies t t2 t3 acc klist
            (List.filter (fun x -> x <> snd h) vlist)
    | _ -> acc
  in
  removed_copies pairings keys values [] keys values

(** [remove_snd grid tt] recurses through the list of tile pairings and removes
    the farthest tile from the grid border from [grid.tiles]. For example, if
    [grid.tiles] is [Tile in position 11; Tile in position 12] and the first
    element of [tt] is (Tile in position 11, Tile in position 12), then the new
    grid produced would have a grid.tiles field that would be
    [Tile in position 11]. Requires: Tile pairings have already been sorted by
    direction (thus ensuring that the second element of the tuple has to be the
    farthest). *)
let remove_snd grid tt =
  let rec removed_grid grid far =
    match far with
    | [] -> grid
    | h :: t -> removed_grid (Grid.remove_tile grid h) t
  in
  removed_grid grid (List.map (fun x -> snd x) tt)

(** [double_fst grid tt] doubles the values of the tiles that are closest to the
    grid border in the tile-neighbor pairing, an element of [tt]. These new
    doubled value tiles are set with Grid.set_tiles to be [grid.tiles].
    Requires: Tile pairings have already been sorted by direction (thus ensuring
    that the first element of the tuple has to be the closest). *)
let double_fst grid tt =
  let close = List.map (fun x -> fst x) tt in
  let rec doubled_grid grid_tiles close acc =
    match grid_tiles with
    | [] -> acc
    | h :: t ->
        if List.mem h close then
          let pos = Tile.pos h in
          let value = Tile.value h in
          doubled_grid t close (Tile.create_tile (value + value) pos [] :: acc)
        else doubled_grid t close (h :: acc)
  in
  let double = doubled_grid (Grid.grid_tiles grid) close [] in
  Grid.set_tiles grid double

(**[combine grid dir] takes a grid [grid] and a command [dir] that indicates the
   direction in which the tiles last moved. If two tiles are neighbors (relative
   to the same direction [dir]) and have the same value, the tile fathest from
   the grid border will be deleted and a new tile will be created with the sum
   of the values. Returns a grid with updated grid.tiles. *)
let combine (grid : Grid.grid) (dir : Tile.direction) : Grid.grid =
  let close_tiles = remove_dups (closer grid dir) in
  double_fst (remove_snd grid close_tiles) close_tiles

(**[update_grid grid command] takes in a grid, moves it based on the command
   passed, combines any touching tiles with the same value and direction, moves
   the combines tiles again, and adds a random tile. *)
let update_grid (grid : Grid.grid) (command : Command.command) : Grid.grid =
  let dir = List.assoc command direction_list in
  let new_grid_one = move_grid command grid |> Grid.set_tiles grid in
  let new_grid_two =
    combine new_grid_one dir |> Grid.grid_tiles |> Grid.set_tiles new_grid_one
  in
  let new_grid_three =
    move_grid command new_grid_two |> Grid.set_tiles new_grid_two
  in
  Grid.grid_tiles new_grid_three
  |> Grid.add_tile new_grid_three
  |> Grid.set_tiles new_grid_three

(**take size of position list, generate a random number in position list size.
   make new tile*)
let update_grid2 (grid : Grid.grid) (command : Command.command) : Grid.grid =
  let new_grid = move_grid command grid |> Grid.set_tiles grid in
  Grid.grid_tiles new_grid |> Grid.add_tile new_grid |> Grid.set_tiles new_grid

let create_state grid int = { grid; score = int }

let sum_values g =
  List.fold_left (fun acc x -> Tile.value x + acc) 0 (Grid.grid_tiles g)

let update_score st = { st with score = st.score + (1 * sum_values st.grid) }
let get_grid s = s.grid
let get_score st = st.score

(*tile compare t1 t2 returns 0 if the Tiles are the same position and positive
  value if t1 has a greateer pos than t2 and a negative number if t2 has a
  greateer pos than t1*)
let tile_compare t1 t2 =
  if Tile.value t1 = Tile.value t2 && Tile.pos t1 = Tile.pos t2 then 0
  else Tile.pos t1 - Tile.pos t2

(**tile_lst_comp lst1 lst2 takes in two lists of tiles and returns true if they
   are the same and false if there is at least one difference. The order of the
   tiles on the list doesn't matter.**)
let tile_lst_comp lst1 lst2 =
  List.sort_uniq tile_compare lst1 = List.sort_uniq tile_compare lst2

(**grid_comp g1 g2 returns false if grids have the same size and tile lists else
   returns true*)
let grid_comp g1 g2 =
  if Grid.grid_size g1 = Grid.grid_size g2 then
    not (tile_lst_comp (Grid.grid_tiles g1) (Grid.grid_tiles g2))
  else true

let check_win g =
  if
    List.length (List.filter (fun x -> Tile.value x = 2048) (Grid.grid_tiles g))
    > 0
  then true
  else false

(*check combine g checks to see if there is any possible way that a player can
  move if there is a direction where moving results in a combination then
  returns true else returns false *)
let check_combine g =
  grid_comp g (combine g Top)
  || grid_comp g (combine g Bottom)
  || grid_comp g (combine g Left)
  || grid_comp g (combine g Right)
