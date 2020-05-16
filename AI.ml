open State

(** [col_ok st col] returns true if column number [col] in gamestate [st] is 
    a valid move (i.e. not full). *)
let col_ok st col : bool = 
  (List.nth st.colDepth col)<6

(** [check_win_situation st col player] returns true if player [player] has won
    after the game board in state [st] has been updated with an added token in 
    the column [col]. Otherwise, it returns false. *)
let check_win_situation st col player : bool = 
  col_ok st col && winner (update_state st col) = player

(** [check_player_win st player] is the column number of a move that
    would allow [player] to win the game, if possible. If no such move exists,
    it returns -1. *)
let check_player_win st player = 
  if (check_win_situation st 0 player) then 0 else 
  if (check_win_situation st 1 player) then 1 else 
  if (check_win_situation st 2 player) then 2 else 
  if (check_win_situation st 3 player) then 3 else 
  if (check_win_situation st 4 player) then 4 else 
  if (check_win_situation st 5 player) then 5 else 
  if (check_win_situation st 6 player) then 6 else (~-1)

(** [check_AI_win st] is the column where player 2 can win in one move 
    from state [st]; if no column exists, -1 is returned.
    Example: If the top row of the board game of [st] is [0 1 2 0 2 2 0 ] 
    and everything underneath is filled, then [check_AI_win st] returns 3. *)
let check_AI_win st = 
  check_player_win st 2

(** [check_opponent_win st] is the column where player 1 can win in one move 
    from state [st]; if no column exists, -1 is returned.
    Example: If the top row of the board game of [st] is [0 2 1 0 1 1 0 ] 
    and everything underneath is filled, then [check_AI_win st] returns 3. *)
let check_opponent_win st = 
  check_player_win (only_change_player st) 1

(** [check_dir st x y row col acc] returns the connection score on
    dirction [x] [y] at point [row, col] in game state [st], with a four in one
    direction to be 100. *)
let rec check_dir st x y row col acc =
  if acc = 3 then 100
  else if row > 5 || row < 0 || col > 6 || col < 0 then acc
  else 
  if st.player1 then
    if get_cell st col row = 1 then check_dir st x y (row-x) (col-y) (acc+1)
    else acc
  else 
  if get_cell st col row = 2 then check_dir st x y (row-x) (col-y) (acc+1)
  else acc

(** [pt_total st row col] returns the total number of connections at a point
    [row, col] if a player puts the coin in column [col] in game state [st]. If 
    the opponent is going to win or the input is invalid, a very negative number
    is returned. *)
let pt_total st row col = 
  if check_opponent_win st<>(-1) then (-10000000)
  else if not (col_ok st col) then (-1000000000)
  else 
  if 
    (check_dir st 0 1 row (col-1) 0) + (check_dir st 0 (-1) row (col+1) 0)=3 ||
    (check_dir st 1 0 (row-1) col 0) + (check_dir st (-1) 0 (row+1) col 0)=3 ||
    (check_dir st 1 1 (row-1) (col-1) 0) +
    (check_dir st (-1) (-1) (row+1) (col+1) 0) = 3 ||
    (check_dir st 1 (-1) (row-1) (col+1) 0) + 
    (check_dir st (-1) 1 (row+1) (col-1) 0) = 3 
  then
    100
  else 
    (check_dir st 0 1 row (col-1) 0) + 
    (check_dir st 1 0 (row-1) col 0) +
    (check_dir st 0 (-1) row (col+1) 0) + 
    (check_dir st (-1) 0 (row+1) col 0) +
    (check_dir st 1 1 (row-1) (col-1) 0) + 
    (check_dir st (-1) (-1) (row+1) (col+1) 0) +
    (check_dir st 1 (-1) (row-1) (col+1) 0) + 
    (check_dir st (-1) 1 (row+1) (col-1) 0)

(** [opt_choice st col (opt_col,opt_connections)] returns the optimal column 
    with the greatest connection score for a player who places their coin in 
    column [col] of the game state [st]. *)
let rec opt_choice st col (opt_col,opt_connections) =
  if col > 6 then (opt_col,opt_connections)
  else 
  if not (col_ok st col) then opt_choice st (col+1) (opt_col,opt_connections)
  else
  if pt_total st (List.nth st.colDepth col) col > opt_connections then
    opt_choice st (col+1) (col,pt_total st (List.nth st.colDepth col) col)
  else opt_choice st (col+1) (opt_col,opt_connections)

(** [minimax_dpth_3 st col] returns the optimal column for the AI with
    the most recent state being [st], which represents the human's coin being 
    placed in the column [col]. *)
let minimax_dpth_3 st col =
  if st.turn+3>42 then 0
  else 
  if (col_ok st col) then
    let newst = update_state st col in
    let oc = opt_choice newst 0 (0,-100) in
    if (snd oc) >= 100 then 10000
    else 10*(snd oc)
  else (-10000000000)

(** [minimax_dpth_2 st col] returns the optimal column for the human with
    the most recent state being [st], which represents the AI's coin being 
    placed in the column [col]. *)
let minimax_dpth_2 st col =
  if st.turn+2>42 then 0
  else 
  if (col_ok st col) then
    (let newst = update_state st col in
     let oc = opt_choice newst 0 (0,-100) in
     if (snd oc) > 100 then (-10000)
     else minimax_dpth_3 newst (fst oc) - 10*(snd oc))
  else (-10000000000)

(** [max_choice (col1,val1) (col2,val2)] returns the column-value tuple with 
    the maximum connection score. If two scores are equal, it randomly chooses 
    one of them to return. *)
let max_choice (col1,val1) (col2,val2) =
  if val1 > val2 then (
    (col1,val1))
  else if val1 < val2 then (
    (col2,val2))
  else 
    (Random.self_init ();
     if Random.int(3) = 1 then (
       (col1,val1))
     else (col2,val2))

(** [penalty_AI st col] returns a large negative number if the human can win 
    in the next turn, if the AI places its coin in the column [col] of the 
    state [st]. Otherwise, it returns 0. *)
let penalty_AI st col = 
  if (check_win_situation st col 1) 
  then (-1000000000000000000)
  else 0

(** Returns the optimal column with the maximum number of connections in all 
    possible directions. *)
let opt_col st = 
  fst (List.fold_left max_choice (0,-10000000) 
         [(0,(penalty_AI st 0)+10*(pt_total st (List.nth st.colDepth 0) 0) + 
             (minimax_dpth_2 st 0));
          (1,(penalty_AI st 1)+10*(pt_total st (List.nth st.colDepth 1) 1) + 
             (minimax_dpth_2 st 1));
          (2,(penalty_AI st 2)+10*(pt_total st (List.nth st.colDepth 2) 2) + 
             (minimax_dpth_2 st 2));
          (3,(penalty_AI st 3)+10*(pt_total st (List.nth st.colDepth 3) 3) + 
             (minimax_dpth_2 st 3));
          (4,(penalty_AI st 4)+10*(pt_total st (List.nth st.colDepth 4) 4) + 
             (minimax_dpth_2 st 4));
          (5,(penalty_AI st 5)+10*(pt_total st (List.nth st.colDepth 5) 5) + 
             (minimax_dpth_2 st 5));
          (6,(penalty_AI st 6)+10*(pt_total st (List.nth st.colDepth 6) 6) + 
             (minimax_dpth_2 st 6));])

(** [play_easy st] returns the column number that the easy AI should place its
    coin in. *)
let rec play_easy st = 
  let move  = Random.int(7) in
  if (List.nth st.colDepth move) <> 6 then move else play_easy st

(** [play_medium st] returns the column number that the medium AI should place 
    its coin in. *)
let play_medium st = 
  let win_col = check_opponent_win st in 
  if (win_col >= 0) then win_col else
    play_easy st

(** [play_hard st] returns the column number that the hard AI should place its
    coin in. *)
let play_hard st = 
  let win_col1 = check_AI_win st in 
  if (win_col1 >= 0) then win_col1 else
    let win_col2 = check_opponent_win st in 
    if (win_col2 >= 0) then win_col2 else
      opt_col st

(** [ai_generated_col st mode] returns the column the AI should place its coin 
    in, depending on the current state [st] and the AI's difficulty 
    level [mode]. *)
let ai_generated_col st mode = 
  match mode with
  | 1 -> play_easy st
  | 2 -> play_medium st
  | 3 -> play_hard st
  | _ -> failwith "impossible"





