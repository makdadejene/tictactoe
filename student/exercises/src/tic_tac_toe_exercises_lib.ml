open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let length_map = Game_kind.board_length game_kind in
  let available_map =
    List.concat_map (List.range 0 length_map) ~f:(fun row ->
      List.map (List.range 0 length_map) ~f:(fun column ->
        { Position.row; Position.column }))
  in
  let filtered_map =
    List.filter available_map ~f:(fun position ->
      not (List.mem (Map.keys pieces) position ~equal:Position.equal))
  in
  filtered_map
;;

(* ignore game_kind; ignore pieces; *)
(* failwith "Implement me!" *)

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

(* let get_all_positions ~(game_kind : Game_kind.t) = let board_length =
   Game_kind.board_length game_kind in List.init board_length ~f:(fun row ->
   List.init board_length ~f:(fun column -> let pos = { Position.row; column
   } in pos)) ;; *)

(* let check_consecutive ~(pieces : Piece.t Position.Map.t) ~(pos :
   Position.t list) ~(count: int) = match pos with | [] -> false | hd:: rest
   -> let rec check_consecutive

   true ;; *)
(* let group get_all_positions ~game_kind (dir: string) = let board_length =
   Game_kind.board_length game_kind in let win_length = Game_kind.win_length
   game_kind in

   match direction with

   | "horizontal" -> List.filter get_all_positions List.init win_length

   ;; *)

let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  match game_kind with
  | Tic_tac_toe ->
    let x_filtered =
      Map.filter pieces ~f:(fun curr_var -> Piece.equal curr_var Piece.X)
    in
    let o_filtered =
      Map.filter pieces ~f:(fun curr_var -> Piece.equal curr_var Piece.O)
    in
    let horizontal map key data =
      match
        Map.find map (Position.right key), Map.find map (Position.left key)
      with
      | Some piece_one, Some piece_two ->
        if Piece.equal piece_one data && Piece.equal piece_two data
        then true
        else false
      | _, _ -> false
    in
    let vertical map key data =
      match
        Map.find map (Position.down key), Map.find map (Position.up key)
      with
      | Some piece_one, Some piece_two ->
        if Piece.equal piece_one data && Piece.equal piece_two data
        then true
        else false
      | _, _ -> false
    in
    let diagonal_one map key data =
      match
        ( Map.find map (Position.right (Position.down key))
        , Map.find map (Position.left (Position.up key)) )
      with
      | Some piece_one, Some piece_two ->
        if Piece.equal piece_one data && Piece.equal piece_two data
        then true
        else false
      | _, _ -> false
    in
    let diagonal_two map key data =
      match
        ( Map.find map (Position.down (Position.right key))
        , Map.find map (Position.up (Position.left key)) )
      with
      | Some piece_one, Some piece_two ->
        if Piece.equal piece_one data && Piece.equal piece_two data
        then true
        else false
      | _, _ -> false
    in
    let win map =
      Map.existsi map ~f:(fun ~key ~data ->
        if Map.mem map key
        then
          horizontal map key data
          || vertical map key data
          || diagonal_one map key data
          || diagonal_two map key data
        else false)
    in
    if win x_filtered
    then Evaluation.Game_over { winner = Some Piece.X }
    else if win o_filtered
    then Evaluation.Game_over { winner = Some Piece.O }
    else Game_continues
  | Omok ->
    let x_filtered =
      Map.filter pieces ~f:(fun curr_var -> Piece.equal curr_var Piece.X)
    in
    let o_filtered =
      Map.filter pieces ~f:(fun curr_var -> Piece.equal curr_var Piece.O)
    in
    let horizontal map key data =
      match
        ( Map.find map (Position.right key)
        , Map.find map (Position.right (Position.right key))
        , Map.find map (Position.left key)
        , Map.find map (Position.left (Position.left key)) )
      with
      | Some piece_one, Some piece_two, Some piece_three, Some piece_four ->
        if Piece.equal piece_one data
           && Piece.equal piece_two data
           && Piece.equal piece_three data
           && Piece.equal piece_four data
        then true
        else false
      | _, _, _, _ -> false
    in
    let vertical map key data =
      match
        ( Map.find map (Position.down key)
        , Map.find map (Position.down (Position.down key))
        , Map.find map (Position.up (Position.up key))
        , Map.find map (Position.up key) )
      with
      | Some piece_one, Some piece_two, Some piece_three, Some piece_four ->
        if Piece.equal piece_one data
           && Piece.equal piece_two data
           && Piece.equal piece_three data
           && Piece.equal piece_four data
        then true
        else false
      | _, _, _, _ -> false
    in
    let diagonal_one map key data =
      match
        ( Map.find map (Position.right (Position.down key))
        , Map.find
            map
            (Position.right
               (Position.down (Position.right (Position.down key))))
        , Map.find map (Position.left (Position.up key))
        , Map.find
            map
            (Position.left (Position.up (Position.left (Position.up key)))) )
      with
      | Some piece_one, Some piece_two, Some piece_three, Some piece_four ->
        if Piece.equal piece_one data
           && Piece.equal piece_two data
           && Piece.equal piece_three data
           && Piece.equal piece_four data
        then true
        else false
      | _, _, _, _ -> false
    in
    let diagonal_two map key data =
      match
        ( Map.find map (Position.down (Position.right key))
        , Map.find
            map
            (Position.down
               (Position.right (Position.down (Position.right key))))
        , Map.find map (Position.up (Position.left key))
        , Map.find
            map
            (Position.up (Position.left (Position.up (Position.left key)))) )
      with
      | Some piece_one, Some piece_two, Some piece_three, Some piece_four ->
        if Piece.equal piece_one data
           && Piece.equal piece_two data
           && Piece.equal piece_three data
           && Piece.equal piece_four data
        then true
        else false
      | _, _, _, _ -> false
    in
    let win map =
      Map.existsi map ~f:(fun ~key ~data ->
        if Map.mem map key
        then
          horizontal map key data
          || vertical map key data
          || diagonal_one map key data
          || diagonal_two map key data
        else false)
    in
    if win x_filtered
    then Evaluation.Game_over { winner = Some Piece.X }
    else if win o_filtered
    then Evaluation.Game_over { winner = Some Piece.O }
    else Game_continues
;;

(* let win_length = Game_kind.win_length game_kind in *)

(* ignore pieces; ignore game_kind; failwith "Implement me!" *)

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let avail_moves = available_moves ~game_kind ~pieces in
  List.filter avail_moves ~f:(fun position ->
    match
      evaluate ~game_kind ~pieces:(Map.set pieces ~key:position ~data:me)
    with
    | Evaluation.Game_over { winner = Some _me } -> true
    | _ -> false)
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let avail_moves = available_moves ~game_kind ~pieces in
  let win_moves = winning_moves ~me:(Piece.flip me) ~game_kind ~pieces in
  match win_moves with
  | [] -> []
  | _ ->
    List.filter avail_moves ~f:(fun position ->
      not (List.mem win_moves position ~equal:Position.equal))
;;

(* ignore me; ignore game_kind; ignore pieces; failwith "Implement me!" *)

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {| 
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
    ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
let%expect_test "evalulate_win_for_x" =
  print_endline
    (evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evalulate_non_win" =
  print_endline
    (evaluate ~game_kind:non_win.game_kind ~pieces:non_win.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| 
   (((row 1) (column 1))) |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect
    {|
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2))
    ((row 2) (column 1))) |}]
;;
