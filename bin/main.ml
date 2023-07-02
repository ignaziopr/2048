open ANSITerminal
open Game
open Grid
open Tile
open State

let example_grid () = Grid.populate_grid (Grid.create_grid 5)

(**[full_read ()] reads the line for commands and prompts the user for a new
   input if they fail to enter Start or Quit or a Up Down Left Right Command*)
let rec full_read st () =
  match Command.parse (read_line ()) with
  | Start ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\
         Oops! Try again! Type 'w' 'a' 's' or 'd' to play or type 'quit' to \
         quit the game!\n";
      full_read st ()
  | Quit ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "\nThanks for playing!\n";
      exit 0
  | Up ->
      (let g = State.get_grid st in
       try
         let new_grid = State.update_grid g Up in
         if State.check_win new_grid then
           ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nYou Won!\n"
         else
           let st = State.update_score st in
           ANSITerminal.print_string [ ANSITerminal.blue ]
             "\n\n\
              Type 'w' 'a' 's' or 'd' to play or type 'quit' to quit the game!\n";
           ANSITerminal.print_string [ ANSITerminal.magenta ]
             ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n");
           print_all_boxes new_grid (grid_size new_grid);
           full_read (State.create_state new_grid (State.get_score st)) ()
       with Full ->
         if State.check_combine g then (
           print_all_boxes g (grid_size g);
           full_read st ())
         else
           ANSITerminal.print_string [ ANSITerminal.red ]
             "\nYou Lost! Thanks for playing!\n");
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n")
  | Down ->
      (let g = State.get_grid st in
       try
         let new_grid = State.update_grid g Down in
         if State.check_win new_grid then
           ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nYou Won!\n"
         else
           let st = State.update_score st in
           ANSITerminal.print_string [ ANSITerminal.blue ]
             "\n\n\
              Type 'w' 'a' 's' or 'd' to play or type 'quit' to quit the game!\n";
           ANSITerminal.print_string [ ANSITerminal.magenta ]
             ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n");
           print_all_boxes new_grid (grid_size new_grid);
           full_read (State.create_state new_grid (State.get_score st)) ()
       with Full ->
         if State.check_combine g then (
           print_all_boxes g (grid_size g);
           full_read st ())
         else
           ANSITerminal.print_string [ ANSITerminal.red ]
             "\nYou Lost! Thanks for playing!\n");
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n")
  | Left ->
      (let g = State.get_grid st in
       try
         let new_grid = State.update_grid g Left in
         if State.check_win new_grid then
           ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nYou Won!\n"
         else
           let st = State.update_score st in
           ANSITerminal.print_string [ ANSITerminal.blue ]
             "\n\n\
              Type 'w' 'a' 's' or 'd' to play or type 'quit' to quit the game!\n";
           ANSITerminal.print_string [ ANSITerminal.magenta ]
             ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n");
           print_all_boxes new_grid (grid_size new_grid);
           full_read (State.create_state new_grid (State.get_score st)) ()
       with Full ->
         if State.check_combine g then (
           print_all_boxes g (grid_size g);
           full_read st ())
         else
           ANSITerminal.print_string [ ANSITerminal.red ]
             "\nYou Lost! Thanks for playing!\n");
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n")
  | Right ->
      (let g = State.get_grid st in
       try
         let new_grid = State.update_grid g Right in
         if State.check_win new_grid then
           ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nYou Won!\n"
         else
           let st = State.update_score st in
           ANSITerminal.print_string [ ANSITerminal.blue ]
             "\n\n\
              Type 'w' 'a' 's' or 'd' to play or type 'quit' to quit the game!\n";
           ANSITerminal.print_string [ ANSITerminal.magenta ]
             ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n");
           print_all_boxes new_grid (grid_size new_grid);
           full_read (State.create_state new_grid (State.get_score st)) ()
       with Full ->
         if State.check_combine g then (
           print_all_boxes g (grid_size g);
           full_read st ())
         else
           ANSITerminal.print_string [ ANSITerminal.red ]
             "\nYou Lost! Thanks for playing!\n");
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        ("\n Score : " ^ string_of_int (State.get_score st) ^ "\n")
  | exception _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\
        \ Oops! Try again! Type 'start' to start or type 'quit' to quit the \
         game!\n";
      full_read st ()

(**[play ()] generates the grid and starting tile, and prints a message
   describing the game controls*)
let play () =
  let gr = example_grid () in
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Inverse ]
    "\nControls: \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n W : Up \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n A: Left \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n S: Down \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n D: Right\n";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "\n Score: 0 \n";
  print_all_boxes gr (grid_size gr);
  full_read (State.create_state gr 0) ()

(**[read ()] reads the line for commands and prompts the user for a new input if
   they fail to enter Start or Quit*)
let rec read () =
  match Command.parse (read_line ()) with
  | Start -> play ()
  | Quit ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "\nThanks for playing!\n";
      exit 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\
         Oops! Try again! Type 'start' to start or type 'quit' to quit the game!\n";
      read ()
  | exception _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\
         Oops! Try again! Type 'start' to start or type 'quit' to quit the game!\n";
      read ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Inverse ]
    "\nWelcome to 2048!\n";
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Inverse ]
    "\nThe goal of this game is to combine the tiles to reach 2048!\n";
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Inverse ]
    "\n\
     You will be able to move the tiles using the 'w', 'a', 's', and 'd' keys.\n";
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Inverse ]
    "\nType 'start' to start or type 'quit' to quit the game!\n";
  read ()

(*let intial_state = State.create_state gr 0*)

(*Examples:*)
(* Execute the game engine. *)
let () = main ()
