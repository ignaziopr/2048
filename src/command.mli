(**Module to handle user input.*)

(**[command] is the type of command the user inputs.*)
type command =
  | Start
  | Quit
  | Up
  | Down
  | Left
  | Right

exception Empty
(**[Empty] is raised when the user's input has no non space characters. *)

exception Malformed
(**[Malformed] is raised when the user's input has non space characters, but the
   input does not form a valid command.*)

val parse : string -> command
(**[parse str] parses the user's input in the terminal [str] and returns the
   command associated with the user's input. Example: parse "start" returns
   [Start]. *)
