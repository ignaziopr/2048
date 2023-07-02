type command =
  | Start
  | Quit
  | Up
  | Down
  | Left
  | Right

exception Empty
exception Malformed

(**[parse str] returns a command based characters in string str. parse ignores
   spaces in the string. 'w' is the key that parses to command Up , 'a' is the
   key that parses to command Left, 's' is the key that parses to command Down,
   'd' is the key that parses to command Right Example parse "start " -> Start*)
let parse str =
  match String.split_on_char ' ' (String.lowercase_ascii str) with
  | [] -> raise Empty
  | h :: t as t2 -> begin
      match List.filter (fun x -> x <> "") t2 with
      | "quit" :: t -> if t = [] then Quit else raise Malformed
      | "start" :: t -> if t = [] then Start else raise Malformed
      | "w" :: t -> if t = [] then Up else raise Malformed
      | "a" :: t -> if t = [] then Left else raise Malformed
      | "s" :: t -> if t = [] then Down else raise Malformed
      | "d" :: t -> if t = [] then Right else raise Malformed
      | [] -> raise Empty
      | _ -> raise Malformed
    end