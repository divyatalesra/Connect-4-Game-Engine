type t = {
  (** [boardGame] is a list of 7 lists, which represents the 7 columns of the 
      current configuration of the 6 by 7 board game. 
      A cell containing 1 represents the presence of player 1's coin. A cell 
      containing 2 represents the presence of player 2's coin. A cell containing 
      0 represents an empty cell. 
      Requires: All the cells contain 0, 1, or 2. 
      Example: 
      [[0, 0, 0, 0, 0, 0], 
      [0, 0, 0, 0, 0, 0], 
      [1, 2, 1, 2, 2, 0], 
      [2, 1, 2, 0, 0, 0], 
      [1, 2, 1, 1, 0, 0], 
      [0, 0, 0, 0, 0, 0], 
      [1, 2, 0, 0, 0, 0]]
      represents the following board game:
      0 0 0 0 0 0 0
      0 0 2 0 0 0 0
      0 0 2 0 1 0 0
      0 0 1 2 1 0 0 
      0 0 2 1 2 0 2
      0 0 1 2 1 0 1
  *)
  boardGame: int list list;

  (** The nth element in [colDepth] represents the row the coin will be placed
      in if it is placed in the nth column of the board game. 
      A cell with 6 represents a full column in which the user will not be able 
      to input any coins. A cell with 0 represents an empty column. 
      Requires: All the cells contain 0, 1, 2, 3, 4, 5, or 6.
      Example: The [colDepth] for the above example is: 
      [0, 0, 5, 3, 4, 0, 2] *)
  colDepth: int list; 

  (** [player1] is true if it is player 1's turn; otherwise, it's false. *)
  player1: bool;

  (** [turn] represents the total number of turns player 1 and player 2 have 
      taken altogether.
      Requires: -1 < [turn] < 43 *)
  turn: int;

  (** [last_move] represents the (row,column) coordintae of the most recent coin
      added. (-1, -1) means that there have been no moves yet. *)
  last_move: int*int;

  (** [prev_st] represents the configuration of the game board exactly one 
      move ago. *)
  prev_st: t option 
}

(** [init_state] represents the starting state of the game. Player 1 goes 
    first. *)
let init_state = 
  {boardGame = 
     [[0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]];
   colDepth = [0; 0; 0; 0; 0; 0; 0];
   player1 = true; 
   turn = 0; 
   last_move = (-1, -1);  
   prev_st = None;
  }

(** [get_cell st col row] returns the value contained in the cell of column
    [col] and row [row] during state [st]. *)
let get_cell st col row =
  List.nth (List.nth st.boardGame col) row

(** [check_direction last_m st horizontal verticle n] returns true if from the 
    point of [last_m] on the direction of ([horizontal],[verticle]) of the board 
    there are [n] coins which are the same value with the current player's index 
    in [st].
    Requires: [n>=0] 
    Raises: [NegativeCoinException] if [n<0] *)
let rec check_direction last_m (st:t) horizontal verticle n =
  if n = 0 then true
  else if fst last_m > 5 || fst last_m < 0 || snd last_m > 6 || snd last_m < 0
  then false
  else let row = fst last_m in let col = snd last_m in
    if st.player1
    then (
      get_cell st col row = 2 && 
      check_direction (row-horizontal,col-verticle) 
        st horizontal verticle (n-1))
    else get_cell st col row = 1 && 
         check_direction (row-horizontal,col-verticle) 
           st horizontal verticle (n-1)

(** [winner st] returns 1 if player 1 has won; 2 if player 2 has won; 
    othwerwise, returns 0. *)
let winner st = 
  let last_m = st.last_move in
  if (check_direction last_m st 1 (~-1) 4)
     ||
     (check_direction last_m st 1 0 4)
     ||
     (check_direction last_m st (~-1) 0 4)
     ||
     (check_direction last_m st (~-1) 1 4)
     ||
     (check_direction last_m st 0 1 4)
     ||
     (check_direction last_m st 1 1 4)
     ||
     (check_direction last_m st (~-1) (~-1) 4)
     ||
     (check_direction last_m st 0 (~-1) 4)
     ||
     ((check_direction last_m st 1 (~-1) 3)
      &&
      (check_direction last_m st (~-1) 1 2))
     ||
     ((check_direction last_m st 1 (~-1) 2)
      &&
      (check_direction last_m st (~-1) 1 3))
     ||
     ((check_direction last_m st (~-1) 0 3)
      &&
      (check_direction last_m st 1 0 2))
     ||
     ((check_direction last_m st (~-1) 0 2)
      &&
      (check_direction last_m st 1 0 3))
     ||
     ((check_direction last_m st 0 (~-1) 3)
      &&
      (check_direction last_m st 0 1 2))
     ||
     ((check_direction last_m st 0 (~-1) 2)
      &&
      (check_direction last_m st 0 1 3))
     ||
     ((check_direction last_m st (~-1) (~-1) 3)
      &&
      (check_direction last_m st 1 1 2))
     ||
     ((check_direction last_m st (~-1) (~-1) 2)
      &&
      (check_direction last_m st 1 1 3))
  then if st.player1 then 2 else 1
  else 0

(** [update_list column player] updates the column [column] in the game board
    by replacing the first element that equals 0 in [column] with 1 if it's 
    player 1's turn or 2 if it's player 2's turn. *)
let update_list column player = 
  match column with 
  |0::t -> player::t
  |[a; 0; b; c; d; e] -> [a; player; b; c; d; e]
  |[a; b; 0; c; d; e] -> [a; b; player; c; d; e]
  |[a; b; c; 0; d; e] -> [a; b; c; player; d; e] 
  |[a; b; c; d; 0; e] -> [a; b; c; d; player; e] 
  |[a; b; c; d; e; 0] -> [a; b; c; d; e; player]
  |_ -> failwith "This case will never be reached."

(** [update_board game col] updates the gameboard in [game] by updating the 
    column [col] using the helper function [update_list column player]. 
    The column [col] represents the column the player, whose turn it is 
    currently, chose to place their piece in. 
    Example:
    If the current [boardGame] is:
    [[0, 0, 0, 0, 0, 0], 
     [0, 0, 0, 0, 0, 0], 
     [1, 2, 1, 2, 2, 0], 
     [2, 1, 2, 0, 0, 0], 
     [1, 2, 1, 1, 0, 0], 
     [0, 0, 0, 0, 0, 0], 
     [1, 2, 0, 0, 0, 0]]
     and player 1 decides to place their piece in the third column, then 
     the [boardGame] would be updated to:
    [[0, 0, 0, 0, 0, 0], 
     [0, 0, 0, 0, 0, 0], 
     [1, 2, 1, 2, 2, 0], 
     [2, 1, 2, 1, 0, 0], 
     [1, 2, 1, 1, 0, 0], 
     [0, 0, 0, 0, 0, 0], 
     [1, 2, 0, 0, 0, 0]] *)
let update_board game col =
  let board = game.boardGame in 
  let player = if game.player1 then 1 else 2 in 
  List.mapi 
    (fun idx column -> if idx=col then (update_list column player) else column) 
    board

(** [update_colDepth colDepth col] is a helper function that updates [colDepth]. 
    It first checks if the column [col] in the game board is full, by checking 
    if the [col]th element in [colDepth] already contains 6 pieces. 
    If so, it raises [ColumnFull]; else, it increments the [col]th element in
    [colDepth] by 1. 
    Raises: [ColumnFull] if the user attempts to place their piece in a column
    that is already full. *)
let update_colDepth (colDepth: int list) (col: int) : int list = 
  List.mapi (fun idx num -> if idx = col then num+1 else num) colDepth 

(** [update_last_move colDepth col] is a helper function that updates the 
    [last_move] in the state of the game, by replacing the column coordinate
    with [col] which represents the column that the current player chose to move 
    thier piece in, and replacing the row coordinate with the approriate row, 
    which is determined by [colDepth]. *)
let update_last_move (colDepth: int list) (col: int) : int*int= 
  ((List.nth colDepth col), col)

(** [update_state] updates the state of the game [game] using the helper 
    functions [update_colDepth colDepth col] and [update_last_move colDepth col]
    after the current player has chosen to place their piece in column [col]. 
    Requires: The user's input [col] is in the range of [0, 6], and the column
    [col] in the board game is not full. *)
let update_state (game: t) (col: int) : t = 
  {
    colDepth = update_colDepth game.colDepth col;
    boardGame = update_board game col;
    player1 = not(game.player1); 
    turn = game.turn+1; 
    last_move = update_last_move game.colDepth col; 
    prev_st = Some game;
  }

(** [only_change_player] returns the state [game] with the player changed. *)
let only_change_player (game: t) : t = 
  { game with player1 = not(game.player1);}

(** [get_turn st] returns the total number of turns that already
    have been played in state [st]. *)
let get_turn (st:t) : int = st.turn


