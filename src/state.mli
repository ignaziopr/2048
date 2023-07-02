(**Module to manage the gameflow, state and score. *)

type t
(**[t] represents the type of the game state.*)

val update_grid : Grid.grid -> Command.command -> Grid.grid
(**[update_grid grid com] takes in a grid [grid] and a command [com] and returns
   the grid after the tiles have been moved according to the command.*)

val update_score : t -> t
(**[update_score st] takes the state of the game [st] and returns a state with
   an updated score *)

val get_score : t -> int
(**[get score st] takes the state of a game [st] and returns the current score
   of the game.*)

val get_grid : t -> Grid.grid
(**[get_grid st] takes the state of a game [st] and returns the current grid for
   the game.*)

val create_state : Grid.grid -> int -> t
(**[create_state g s] creates a state with grid [g] representing the board and
   score [s].*)

val check_combine : Grid.grid -> bool
(**[check_combine g] returns true if it is possible to combine tiles by moving
   the grid [g] in at least one direction returns false otherwise.*)

val check_win : Grid.grid -> bool
(**[check_win g] returns true if at least one tile in [g] has a value of 2048
   returns false otherwise. *)
