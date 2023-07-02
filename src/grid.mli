(**Module to handle grid creation and grid printing.*)

type grid
(** Type [grid] represents a grid in the game.*)

exception Full
(**[Full] is raised when the board is full of tiles.*)

val create_grid : int -> grid
(** [create_grid dim] creates a new grid with [dim] rows and columns. Requires:
    [dim] is greater than 3. Example: [create_grid 4] creates a 4 x 4 grid. *)

val populate_grid : grid -> grid
(** [populate_grid grid] randomly generates a filled position in the grid [grid]
    and adds that to the list of filled positions.*)

val empty_list : grid -> int list
(**[empty_list grid] returns a list of empty positions in grid [grid]. If there
   are none, it returns an empty list.*)

val filled_list : grid -> int list
(**[filled_list grid] returns a list of filled positions in grid [grid]. If
   there are none, it returns an empty list.*)

val grid_size : grid -> int
(**[grid_size g] returns the length of one side of the grid [g].*)

val add_tile : grid -> Tile.tile list -> Tile.tile list
(**[add_tile grid] generates a new tile and adds it to an available position in
   [grid]. This tile is added to the grid's tiles. If the grid does not have any
   empty tiles, [add_tile grid] returns input grid. Raises: [Full] if the list
   of empty positions is empty.*)

val remove_tile : grid -> Tile.tile -> grid
(**[remove_tile grid t] returns [grid] with tile [t] removed from the grid's
   tiles. *)

val grid_tiles : grid -> Tile.tile list
(**[grid_tiles g] returns the list of tiles in grid [g].*)

val set_tiles : grid -> Tile.tile list -> grid
(**[set_tiles g lst] returns grid [g] with [lst] as its list of Tiles.*)

val print_all_boxes : grid -> int -> unit
(**[print_all_boxes g s] prints the grid corresponding to [g] with a size of
   [s].*)
