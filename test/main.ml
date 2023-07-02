open OUnit2
open Game
open Grid
open State
open Tile

open Command
(**Comment detailing testing approach: In our test suite, we used blackbox
   testing to test our exposed functions for their desired functionality. We
   divided these tests into move tests, parse tests, combine tests, and check
   tests. Parse tests checked the user interaction with the game, to ensure that
   commands were read correctly. Move tests checked a primary part of the game
   functionality, how the tiles move both when they are isolated and in larger
   groups, making sure that our edge detection works. Combine tests checked
   another key piece of the game's functionality, how the tiles combine when
   they are moved into other tiles that have the same value. Finally the check
   tests, check to make sure the winning and losing conditions are properly
   functioning. All test cases were developed as black box tests and we
   attempted to be as thorough as possible, using a variety of inputs given the
   exposed functions we had acess to. We omitted from testing the more visual
   aspects of our code suite, including the many print functions that we have.
   This is because we manually tested this part of the code by playing the game.
   Additionally, we tested the score component of the game manually by playing
   the game. Given the random generation of tiles and their values, many of the
   playable features needed to be tested manually or tested via the properties
   as we explained above. We tested the Tile, State, and Command functions
   directly in the test suite and the Grid module indirectly through the usage
   of its exposed functions. Our test suite demonstrates the correctness of this
   system by testing the key functional aspects (move,combine, parse) and
   showing that they behave correctly under a variety of conditions.
   Additionally, by black box testing we testing in the way that the client of
   the game would see, which means we tested the extent to which a client could
   see and interact with the game.*)

(**[parse_test name expected_output input] constructs an OUnit test named [name]
   that asserts the equality of [expected_ouput] with [parse input]. *)
let parse_test (name : string) (expected_output : command) (input : string) =
  name >:: fun _ -> assert_equal expected_output (parse input)

(**[parse_test_exn name expected_output input] constructs an OUnit test named
   [name] that asserts the equality of [expected_exn] with [parse exn]. *)
let parse_exn (name : string) (expected_exn : exn) (input : string) =
  name >:: fun _ -> assert_raises expected_exn (fun () -> parse input)

(**[pos_lst tlst] returns an int list of all the positions of the tiles in
   [tlst] in sorted ascending order. *)
let pos_lst (tlst : tile list) =
  List.map (fun t -> pos t) tlst |> List.sort Stdlib.compare

(**[val_lst tlst] returns an int list of all the values of the tiles in [tlst]
   in sorted ascending order based on the tile positions.*)
let val_lst (tlst : tile list) =
  List.map (fun t -> value t) (List.sort compare tlst)

(**[move_test name expected_out input_tiles size] constructs and OUnit test
   named [name] that asserts the equality of [expected_out] with
   [move com tlst size].*)
let move_test (name : string) (expected_out : int list)
    (input_tiles : tile list) (in_com : command) (size : int) =
  name >:: fun _ ->
  assert_equal expected_out (move in_com input_tiles size |> pos_lst)

(**[arrange_neighbor_out nlst] arranges the neighbor list [nlst] from a tile
   into a tuple with an int and direction so it can be easily tested in the test
   skeleton. *)
let rec arrange_neighbor_out = function
  | [] -> []
  | (d, op) :: t -> (
      match op with
      | None -> arrange_neighbor_out t
      | Some ti -> (pos ti, d) :: arrange_neighbor_out t)

let string_of_dir (d : direction) =
  match d with
  | Left -> "Left"
  | Right -> "Right"
  | Top -> "Top"
  | Bottom -> "Bottom"
  | Neither -> "Neither"

let int_dir_printer (dir_int_lst : (int * direction) list) =
  List.fold_right
    (fun (i, d) acc ->
      "int: " ^ string_of_int i ^ " dir: " ^ string_of_dir d ^ "; " ^ acc)
    dir_int_lst ""

(**[pos_val_from_tiles tlst] arranges the tile list [tlst] from tiles into a
   list of int * int tuples representing the position and values of each tile
   respectively so that they can be easily tested in the skeleton.*)
let rec pos_val_from_tiles (tlst : tile list) =
  match tlst with
  | [] -> []
  | h :: t -> (pos h, value h) :: pos_val_from_tiles t

(**[arrange_combine_out g] extracts the tiles from the grid, sorts them and
   arranges them into tuples of ints so they can be easily tested in the
   skeleton.*)
let rec arrange_combine_out (g : grid) =
  List.sort compare (grid_tiles g) |> pos_val_from_tiles

(**[neighbor_test name expected_out input_tiles size] constructs an OUnit test
   named [name] that asserts the equality of [expected_out] with
   [update_neighbors pos tile_lst size].*)
let neighbor_test (name : string) (expected_out : (int * direction) list)
    (input_pos : int) (input_tlst : tile list) (size : int) =
  name >:: fun _ ->
  assert_equal expected_out
    (update_neighbors input_pos input_tlst size |> arrange_neighbor_out)
    ~printer:int_dir_printer

let rec is_sub_list (sublst : (int * int) list) (olst : (int * int) list) : bool
    =
  List.fold_left (fun acc (p, v) -> List.mem (p, v) olst && acc) true sublst

(**[combine_test name expected_out input_tiles size] constructs an OUnit test
   named [name] that asserts the equality of [expected_out] with
   [update_grid gr com].*)
let combine_test (name : string) (expected_out : (int * int) list)
    (input_tiles : tile list) (com : command) (size : int) =
  name >:: fun _ ->
  assert_equal true
    (is_sub_list expected_out
       (update_grid (set_tiles (create_grid 5) input_tiles) com
       |> arrange_combine_out))
    ~printer:string_of_bool

(**[combine_test name expected_out input_tiles size] constructs an OUnit test
   named [name] that asserts the equality of [expected_exn] with [].*)
let combine_exn (name : string) (expected_exn : exn) (input_tiles : tile list)
    (com : command) (size : int) =
  name >:: fun _ ->
  assert_raises expected_exn (fun () ->
      update_grid (set_tiles (create_grid 5) input_tiles) com)

(**[check_combine_test name expected_out grid ] constructs an OUnit test named
   [name] that asserts the equality of [expected_out] with [check_combine grid].*)
let check_combine_test (name : string) (expected_out : bool) (g : grid) =
  name >:: fun _ ->
  assert_equal expected_out (State.check_combine g) ~printer:string_of_bool

let check_win_test (name : string) (expected_out : bool) (g : grid) =
  name >:: fun _ ->
  assert_equal expected_out (State.check_win g) ~printer:string_of_bool

let twenty_five_lst : int list =
  [
    1;
    2;
    3;
    4;
    5;
    6;
    7;
    8;
    9;
    10;
    11;
    12;
    13;
    14;
    15;
    16;
    17;
    18;
    19;
    20;
    21;
    22;
    23;
    24;
    25;
  ]

let full_tile_grid = List.map (fun i -> create_tile 2 i []) twenty_five_lst

let full_tile_grid2 =
  List.fold_left (fun acc i -> create_tile i i [] :: acc) [] twenty_five_lst

let full_tile_grid3 =
  List.fold_left (fun acc i -> create_tile 2048 i [] :: acc) [] twenty_five_lst

let move_tests =
  [
    move_test "empty grid moving left" [] [] Left 5;
    move_test "empty grid moving up" [] [] Up 5;
    move_test "empty grid moving down" [] [] Down 5;
    move_test "empty grid moving right" [] [] Right 5;
    move_test "grid with one tile moving left" [ 21 ]
      [ create_tile 2 21 [] ]
      Left 5;
    move_test "grid with one tile moving right" [ 5 ]
      [ create_tile 4 4 [] ]
      Right 5;
    move_test "grid with one tile moving up" [ 3 ]
      [ create_tile 600 13 [] ]
      Up 5;
    move_test "grid with one tile moving down" [ 23 ]
      [ create_tile 40 23 [] ]
      Down 5;
    move_test "grid with one tile moving down but no move" [ 24 ]
      [ create_tile 2 9 [] ]
      Down 5;
    move_test "grid with many tiles in column 2 moving up" [ 2; 7; 12; 17 ]
      [
        create_tile 2 7 [];
        create_tile 4 17 [];
        create_tile 4 12 [];
        create_tile 2 22 [];
      ]
      Up 5;
    move_test "grid with many tiles in column 4 moving down" [ 9; 14; 19; 24 ]
      [
        create_tile 2 4 [];
        create_tile 4 14 [];
        create_tile 2 19 [];
        create_tile 2 24 [];
      ]
      Down 5;
    move_test "grid with many tiles in row 3 moving left" [ 11; 12; 13; 14 ]
      [
        create_tile 2 12 [];
        create_tile 2 15 [];
        create_tile 4 14 [];
        create_tile 4 11 [];
      ]
      Left 5;
    move_test "grid with many tiles in row 2 moving right" [ 7; 8; 9; 10 ]
      [
        create_tile 2 8 [];
        create_tile 4 7 [];
        create_tile 4 6 [];
        create_tile 3 10 [];
      ]
      Right 5;
    move_test "grid with many tiles in many rows moving left"
      [ 1; 2; 6; 7; 11; 12; 13; 16; 17; 21; 22; 23; 24 ]
      [
        create_tile 2 2 [];
        create_tile 2 4 [];
        create_tile 4 11 [];
        create_tile 4 12 [];
        create_tile 4 14 [];
        create_tile 4 18 [];
        create_tile 4 20 [];
        create_tile 4 8 [];
        create_tile 4 10 [];
        create_tile 2 22 [];
        create_tile 4 24 [];
        create_tile 4 23 [];
        create_tile 4 21 [];
      ]
      Left 5;
    move_test "grid with many tiles in many rows moving right"
      [ 4; 5; 9; 10; 13; 14; 15; 19; 20; 22; 23; 24; 25 ]
      [
        create_tile 2 2 [];
        create_tile 2 4 [];
        create_tile 4 11 [];
        create_tile 4 12 [];
        create_tile 4 14 [];
        create_tile 4 18 [];
        create_tile 4 20 [];
        create_tile 4 8 [];
        create_tile 4 10 [];
        create_tile 2 22 [];
        create_tile 4 24 [];
        create_tile 4 23 [];
        create_tile 4 21 [];
      ]
      Right 5;
    move_test "grid with many tiles in many columns moving up"
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 12; 13; 14 ]
      [
        create_tile 2 2 [];
        create_tile 2 4 [];
        create_tile 4 11 [];
        create_tile 4 12 [];
        create_tile 4 14 [];
        create_tile 4 18 [];
        create_tile 4 20 [];
        create_tile 4 8 [];
        create_tile 4 10 [];
        create_tile 2 22 [];
        create_tile 4 24 [];
        create_tile 4 23 [];
        create_tile 4 21 [];
      ]
      Up 5;
    move_test "grid with many tiles in many columns moving down"
      [ 12; 13; 14; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25 ]
      [
        create_tile 2 2 [];
        create_tile 2 4 [];
        create_tile 4 11 [];
        create_tile 4 12 [];
        create_tile 4 14 [];
        create_tile 4 18 [];
        create_tile 4 20 [];
        create_tile 4 8 [];
        create_tile 4 10 [];
        create_tile 2 22 [];
        create_tile 4 24 [];
        create_tile 4 23 [];
        create_tile 4 21 [];
      ]
      Down 5;
    move_test "full grid moving left" twenty_five_lst full_tile_grid Left 5;
    move_test "full grid moving right" twenty_five_lst full_tile_grid Right 5;
    move_test "full grid moving up" twenty_five_lst full_tile_grid Up 5;
    move_test "full grid moving down" twenty_five_lst full_tile_grid Down 5;
  ]

let neighbor_tests =
  [
    neighbor_test "neighbors of one tile with no neighbors" [] 8 [] 5;
    neighbor_test "neighbors of one tile with one left neighbor"
      [ (7, Left) ]
      8
      [ create_tile 2 7 [] ]
      5;
    neighbor_test "neighbors of one tile with one right neighbor"
      [ (9, Right) ]
      8
      [ create_tile 4 9 [] ]
      5;
    neighbor_test "neighbors of one tile with a top neighbor"
      [ (3, Top) ]
      8
      [ create_tile 2 3 [] ]
      5;
    neighbor_test "neighbors of one tile with a bottom neighbor"
      [ (13, Bottom) ]
      8
      [ create_tile 4 13 [] ]
      5;
    neighbor_test "neighbors of one tile against the top wall"
      [ (7, Bottom) ]
      2
      [ create_tile 3 7 [] ]
      5;
    neighbor_test "neighbors of one tile against the bottom wall"
      [ (25, Right) ]
      24
      [ create_tile 2 25 [] ]
      5;
    neighbor_test "neighbors of a tile in the upper left corner"
      [ (2, Right); (6, Bottom) ]
      1
      [ create_tile 2 2 []; create_tile 4 6 [] ]
      5;
    neighbor_test "neighbors of a tile in the upper right hand corner"
      [ (4, Left); (10, Bottom) ]
      5
      [ create_tile 2 4 []; create_tile 4 10 [] ]
      5;
    neighbor_test "neighbors of a tile in the lower left hand corner"
      [ (22, Right); (16, Top) ]
      21
      [ create_tile 2 16 []; create_tile 2 22 [] ]
      5;
    neighbor_test "neighbors of a tile in the lower right hand corner"
      [ (24, Left); (20, Top) ]
      25
      [ create_tile 2 20 []; create_tile 3 24 [] ]
      5;
    neighbor_test "neighbors of a tile in the middle with all 4 neighbors"
      [ (12, Left); (14, Right); (8, Top); (18, Bottom) ]
      13
      [
        create_tile 2 8 [];
        create_tile 2 12 [];
        create_tile 2 14 [];
        create_tile 2 18 [];
      ]
      5;
  ]

let combine_tests =
  [
    combine_test "combining left for one row"
      [ (6, 8) ]
      [ create_tile 4 6 []; create_tile 4 8 [] ]
      Left 5;
    combine_test "combining right for one row"
      [ (10, 8) ]
      [ create_tile 4 6 []; create_tile 4 8 [] ]
      Right 5;
    combine_test "combining up for one column"
      [ (3, 4) ]
      [ create_tile 2 8 []; create_tile 2 23 [] ]
      Up 5;
    combine_test "combining down for one column"
      [ (23, 4) ]
      [ create_tile 2 8 []; create_tile 2 23 [] ]
      Down 5;
    combine_test "combining down for multiple columns"
      [ (17, 2); (20, 4); (21, 4); (22, 4); (23, 8); (24, 2); (25, 2) ]
      [
        create_tile 2 2 [];
        create_tile 4 5 [];
        create_tile 4 6 [];
        create_tile 4 8 [];
        create_tile 2 12 [];
        create_tile 2 14 [];
        create_tile 2 15 [];
        create_tile 2 17 [];
        create_tile 4 23 [];
      ]
      Down 5;
    combine_test "combining up for multiple columns"
      [ (1, 4); (2, 4); (3, 8); (4, 2); (5, 4); (7, 2); (10, 2) ]
      [
        create_tile 2 2 [];
        create_tile 4 5 [];
        create_tile 4 6 [];
        create_tile 4 8 [];
        create_tile 2 12 [];
        create_tile 2 14 [];
        create_tile 2 15 [];
        create_tile 2 17 [];
        create_tile 4 23 [];
      ]
      Up 5;
    combine_test "combining left for multiple rows of tiles"
      [ (1, 2); (2, 4); (6, 8); (11, 4); (12, 2); (16, 2); (21, 4) ]
      [
        create_tile 2 2 [];
        create_tile 4 5 [];
        create_tile 4 6 [];
        create_tile 4 8 [];
        create_tile 2 12 [];
        create_tile 2 14 [];
        create_tile 2 15 [];
        create_tile 2 17 [];
        create_tile 4 23 [];
      ]
      Left 5;
    combine_test "combine right for multiple rows of tiles"
      [ (4, 2); (5, 4); (10, 8); (14, 2); (15, 4); (20, 2); (25, 4) ]
      [
        create_tile 2 2 [];
        create_tile 4 5 [];
        create_tile 4 6 [];
        create_tile 4 8 [];
        create_tile 2 12 [];
        create_tile 2 14 [];
        create_tile 2 15 [];
        create_tile 2 17 [];
        create_tile 4 23 [];
      ]
      Right 5;
    combine_test "combine on a full grid going up"
      [
        (1, 4);
        (2, 4);
        (3, 4);
        (4, 4);
        (5, 4);
        (6, 4);
        (7, 4);
        (8, 4);
        (9, 4);
        (10, 4);
        (11, 2);
        (12, 2);
        (13, 2);
        (14, 2);
        (15, 2);
      ]
      full_tile_grid Up 5;
    combine_test "combine on a full grid going down"
      [
        (11, 2);
        (12, 2);
        (13, 2);
        (14, 2);
        (15, 2);
        (16, 4);
        (17, 4);
        (18, 4);
        (19, 4);
        (20, 4);
        (21, 4);
        (22, 4);
        (23, 4);
        (24, 4);
        (25, 4);
      ]
      full_tile_grid Down 5;
    combine_test "combine on a full grid going left"
      [
        (1, 4);
        (6, 4);
        (11, 4);
        (16, 4);
        (21, 4);
        (2, 4);
        (7, 4);
        (12, 4);
        (17, 4);
        (22, 4);
        (3, 2);
        (8, 2);
        (13, 2);
        (18, 2);
        (23, 2);
      ]
      full_tile_grid Left 5;
    combine_test "combine on a full grid going right"
      [
        (5, 4);
        (10, 4);
        (15, 4);
        (20, 4);
        (25, 4);
        (4, 4);
        (9, 4);
        (14, 4);
        (19, 4);
        (24, 4);
        (3, 2);
        (8, 2);
        (13, 2);
        (18, 2);
        (23, 2);
      ]
      full_tile_grid Right 5;
    combine_exn
      "combine on a full grid going up where each tile has a different value"
      Full full_tile_grid2 Up 5;
    combine_exn
      "combine on a full grid going down where each tile has a different value"
      Full full_tile_grid2 Down 5;
    combine_exn
      "combine on a full grid going left where each tile has a different value"
      Full full_tile_grid2 Left 5;
    combine_exn
      "combine on a full grid going right where each tile has a different value"
      Full full_tile_grid2 Right 5;
  ]

let parse_tests =
  [
    parse_test "Valid start command" Start "start";
    parse_test "Valid Up command" Up "w";
    parse_test "Valid Down command" Down "s";
    parse_test "Valid Left command" Left "a";
    parse_test "Valid Right command" Right "d";
    parse_exn "Empty command" Empty "      ";
    parse_exn "Malformed command" Malformed "  jaksj    ";
  ]

let check_tests =
  [
    check_combine_test "grid with all twos" true
      (set_tiles (create_grid 5) full_tile_grid);
    check_combine_test "grid with all different numbers" false
      (set_tiles (create_grid 5) full_tile_grid2);
    check_combine_test "grid with all different numbers" true
      (set_tiles (create_grid 5) full_tile_grid3);
    check_win_test "grid with all twos numbers, not 2048" false
      (set_tiles (create_grid 5) full_tile_grid);
    check_win_test "grid with all different numbers, not 2048" false
      (set_tiles (create_grid 5) full_tile_grid2);
    check_win_test "grid with all different numbers, not 2048" true
      (set_tiles (create_grid 5) full_tile_grid3);
  ]

let suite =
  "test suite for 2048"
  >::: List.flatten
         [ parse_tests; move_tests; neighbor_tests; combine_tests; check_tests ]

let _ = run_test_tt_main suite
