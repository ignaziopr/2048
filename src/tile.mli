(**Module to handle tile interactions and movement.*)

(**[type direction] is the abstract type representing the direction of a tile's
   neighbor.*)
type direction =
  | Right
  | Left
  | Top
  | Bottom
  | Neither

type tile
(**[type tile] is the abstract type representing a tile. *)

val value : tile -> int
(**[value tile] is the current value of the tile. *)

val pos : tile -> int
(**[pos tile] is the current position of the tile in the grid based on the 1 ..
   25 gird numbering system.*)

val get_neighbors_pertile :
  int -> tile list -> int -> tile * (direction * tile option) list
(**[get_neighbors_pertile pos tiles size] returns all the neighbors of the tile
   with position [pos] given tiles [tiles] in the grid with size [size]. *)

val create_tile : int -> int -> (direction * tile option) list -> tile
(**[create_tile value position neighbors] returns a new tile of value [value] at
   position [position] with neighbors [neighbors]. Requires: [neighbors] is a
   list of valid direction * tile option tuples. *)

val move : Command.command -> tile list -> int -> tile list
(**[move com tile_list size] moves all tiles on the grid given by [tile_list]
   based on input command [com] and the size of the grid [size]. If the tiles
   cannot move, the same tile list as was inputted is returned. *)

val update_neighbors : int -> tile list -> int -> (direction * tile option) list
(**[update_neighbors t t_lst] returns a new tile identical to [t] with the
   neighbors specified [t_lst]. Requires: [t] is a valid tile and [t_lst] is a
   valid tile list.*)
