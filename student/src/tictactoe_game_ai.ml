open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  List.random_element_exn
    (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces)
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  match Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces with
  | [] -> random_move_strategy ~game_kind ~pieces
  | _ ->
    List.random_element_exn
      (Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces)
;;

(* ignore me; ignore game_kind; ignore pieces; failwith "Implement me!" *)

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let avail_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  match Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces with
  | [] ->
    (match Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces with
     | [] -> pick_winning_move_if_possible_strategy ~me ~game_kind ~pieces
     | _ ->
       List.random_element_exn
         (List.filter avail_moves ~f:(fun position ->
            not
              (List.mem
                 (Tic_tac_toe_exercises_lib.losing_moves
                    ~me
                    ~game_kind
                    ~pieces)
                 position
                 ~equal:Position.equal))))
  | _ ->
    List.random_element_exn
      (Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces)
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let curr = Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces in
  match curr with
  | Tic_tac_toe_exercises_lib.Evaluation.Game_over { winner = Some x } ->
    if Piece.equal x me then Float.infinity else Float.neg_infinity
  | _ -> 0.0
;;

(* let _ = score *)

let rec minimax
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(game_status : Game_status.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(curr_score : Position.t list)
  ~(depth : int)
  : float
  =
  let curr = Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces in
  match curr with
  | Tic_tac_toe_exercises_lib.Evaluation.Game_over { winner = Some _x } ->
    score ~me ~game_kind ~pieces
  | _ ->
    if depth = 0
    then score ~me ~game_kind ~pieces
    else (
      let avail_moves =
        Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
      in
      let curr_score =
        List.map avail_moves ~f:(fun position ->
          let curr_piece =
            match game_status with
            | Turn_of piece -> piece
            | _ -> failwith ""
          in
          let new_pieces = Map.set pieces ~key:position ~data:curr_piece in
          minimax
            ~me
            ~game_kind
            ~game_status:(Game_status.Turn_of (Piece.flip me))
            ~pieces:new_pieces
            ~curr_score
            ~depth:(depth - 1))
      in
      if Game_status.equal game_status (Game_status.Turn_of me)
      then (
        match List.max_elt curr_score ~compare:Float.compare with
        | Some p -> p
        | None -> failwith "No1")
      else (
        match List.min_elt curr_score ~compare:Float.compare with
        | Some p -> p
        | None -> failwith "No2"))
;;

let _ = minimax

(* - pass in me, game_kind, pieces, current Player - check if game is over:
   match with score - if Player = me then create a variable called val = -
   infin then recursively go through all available moves - - if Player =
   other player then create a variable call val = + infin then recursively go
   through available moves to determine value *)

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  (* pick_winning_move_or_block_if_possible_strategy ~me
     ~game_kind:game_state.game_kind ~pieces:game_state.pieces ;; *)
  let avail_moves =
    Tic_tac_toe_exercises_lib.available_moves
      ~game_kind:game_state.game_kind
      ~pieces:game_state.pieces
  in
  let curr_map =
    List.map avail_moves ~f:(fun position ->
      let new_pieces = Map.set game_state.pieces ~key:position ~data:me in
      ( position
      , minimax
          ~me
          ~game_kind:game_state.game_kind
          ~game_status:game_state.game_status
          ~pieces:new_pieces
          ~curr_score:[]
          ~depth:3 ))
  in
  match
    List.max_elt
      curr_map
      ~compare:(fun (_move_one, score_one) (_move_two, score_two) ->
      Float.compare score_one score_two)
  with
  | Some (x, _y) -> x
  | None -> failwith "No3"
;;

(* ignore game_state; { Position.row = 0; column = 0 } *)
