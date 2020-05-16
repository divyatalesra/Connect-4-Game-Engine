open OUnit2
open State
open Main

let check_states_eq s1 s2 =
  s1.boardGame = s2.boardGame &&
  s1.colDepth = s2.colDepth &&
  s1.player1 = s2.player1 &&
  s1.turn = s2.turn &&
  s1.last_move = s2.last_move

(* The test cases *)
let initial_state = State.init_state

(* empty column*)
let zero = [0;0;0;0;0;0]

(* A player just won such that they placed their piece
   in the middle of a row: 1 1 (1) 1 *)
let problem_child_row = 
  {boardGame = [
      [1;2;1;2;1;2];
      [1;2;1;2;1;2];
      [1;0;0;0;0;0];
      [1;2;1;2;1;2];
      zero;zero;zero;
    ];colDepth = [6;6;1;6;0;0;0];
   player1 = false; turn =19; last_move = (0,2);
   prev_st = None
  }

(* A player just won such that they placed their piece
   in the middle of a diagonal*)
let problem_child_diag = 
  {boardGame = [
      zero;
      [2;2;2;1;1;0];
      [2;2;1;1;0;0];
      [1;1;1;0;0;0];
      [2;1;0;0;0;0];
      zero;zero
    ];colDepth = [0;5;4;3;2;0;0];
   player1 = false; turn =14; last_move = (2,3);
   prev_st = None
  }

(* A player wins with the following arrangement:
   1 1 (1) 1 1   *)
let connect_five =
  {boardGame = [
      [2;0;0;0;0;0];
      [2;2;2;1;1;0];
      [2;2;1;1;0;0];
      [1;1;1;0;0;0];
      [2;1;0;0;0;0];
      [1;0;0;0;0;0];zero
    ];colDepth = [0;5;4;3;2;1;0];
   player1 = false; turn =16; last_move = (2,3);
   prev_st = None
  }

let right_diag : State.t =
  {boardGame = 
     [[1;1;2;0;0;0];
      [1;1;1;2;0;0];
      [2;1;2;1;2;0];
      [1;2;2;1;2;2];
      [0;0;0;0;0;0];
      [0;0;0;0;0;0];
      [0;0;0;0;0;0]];
   colDepth = [3;4;5;6;0;0;0];
   player1 = true;
   turn = 18;
   last_move = (5,3);
   prev_st = None
  }

let right_diag2 : State.t =
  {
    boardGame = [
      [1;1;1;0;0;0];
      [1;1;2;2;2;0];
      [2;2;1;2;0;0];
      [2;1;2;0;0;0];
      [1;2;0;0;0;0];
      [1;0;0;0;0;0];
      [0;0;0;0;0;0]

    ];
    colDepth = [3;5;4;3;2;1;0];
    player1=true;
    turn =18;
    last_move = (3,2);
    prev_st = None
  }

let examp : State.t = 
  {boardGame = 
     [[1; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]];
   colDepth = [1; 0; 0; 0; 0; 0; 0];
   player1 = false;
   turn = 1; 
   last_move = (0,0);
   prev_st = Some initial_state  
  }
let examp2 : State.t = 
  {boardGame = 
     [[1; 0; 0; 0; 0; 0]; [2; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]];
   colDepth = [1; 1; 0; 0; 0; 0; 0];
   player1 = true; 
   turn = 2; 
   last_move = (0,1);
   prev_st = Some examp  
  }
(* hello *)

let examp3 : State.t = 
  {boardGame = 
     [[1; 1; 0; 0; 0; 0]; [2; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]];
   colDepth = [2; 1; 0; 0; 0; 0; 0];
   player1 = false; 
   turn = 3; 
   last_move = (1,0);
   prev_st = Some examp2   
  }
(* no winner *)
let state1 =
  {boardGame = 
     [[0; 0; 0; 0; 0; 0]; 
      [2; 0; 0; 0; 0; 0]; 
      [1; 1; 2; 0; 0; 0];
      [1; 2; 1; 0; 0; 0]; 
      [1; 1; 2; 0; 0; 0]; 
      [2; 0; 0; 0; 0; 0];
      [2; 0; 0; 0; 0; 0]];
   colDepth = [0; 1; 3; 3; 3; 1;1];
   player1 = true; 
   turn = 12; 
   last_move = (6, 0);
   prev_st = None   
  }


(* Making the Test functions*)
(* player1 wins *)
let state2 =
  {boardGame = 
     [[0; 0; 0; 0; 0; 0]; 
      [2; 0; 0; 0; 0; 0]; 
      [1; 1; 2; 0; 0; 0];
      [1; 2; 1; 0; 0; 0]; 
      [1; 1; 2; 1; 0; 0]; 
      [2; 1; 2; 2; 1; 0];
      [2; 0; 0; 0; 0; 0]];
   colDepth = [0; 1; 3; 3; 3; 5; 1];
   player1 = false; 
   turn = 16; 
   last_move = (4, 5);
   prev_st = None   
  }

let row_win = 
  {boardGame = 
     [[1; 0; 0; 0; 0; 0]; 
      [1; 0; 0; 0; 0; 0];
      [1; 0; 0; 0; 0; 0];
      [1; 0; 0; 0; 0; 0];
      [2; 0; 0; 0; 0; 0];
      [2; 0; 0; 0; 0; 0];
      [2; 0; 0; 0; 0; 0]];
   colDepth = [1; 1; 1; 1; 1; 1; 1];
   player1 = true; 
   turn = 7; 
   last_move = (0, 0);
   prev_st = None   
  }

let row_win_2 = 
  {boardGame = 
     [[2; 0; 0; 0; 0; 0]; 
      [1; 0; 0; 0; 0; 0];
      [2; 1; 0; 0; 0; 0];
      [1; 1; 0; 0; 0; 0];
      [2; 1; 0; 0; 0; 0];
      [2; 1; 0; 0; 0; 0];
      [2; 0; 0; 0; 0; 0]];
   colDepth = [1; 1; 2; 2; 2; 2; 1];
   player1 = true; 
   turn = 11; 
   last_move = (6, 1);
   prev_st = None   
  }
(* player2 wins, column *)
let state3 =
  {boardGame = 
     [[0; 0; 0; 0; 0; 0]; 
      [2; 0; 0; 0; 0; 0]; 
      [1; 1; 2; 0; 0; 0];
      [1; 2; 1; 0; 0; 0]; 
      [1; 1; 2; 1; 0; 0]; 
      [2; 2; 2; 2; 0; 0];
      [2; 0; 0; 0; 0; 0]];
   colDepth = [0; 1; 3; 3; 3; 4; 1];
   player1 = true; 
   turn = 15; 
   last_move = (3, 5);
   prev_st = None   
  }

(* player 1 wins with left diagonal *)
let player1winsLD : State.t = 
  {boardGame = 
     [[1; 2; 0; 0; 0; 0]; 
      [1; 2; 0; 0; 0; 0]; 
      [2; 2; 1; 1; 0; 0];
      [2; 1; 1; 1; 2; 0]; 
      [2; 1; 0; 0; 0; 0]; 
      [1; 2; 0; 0; 0; 0];
      [0; 0; 0; 0; 0; 0]];
   colDepth = [2; 2; 4; 5; 2; 2; 0];
   player1 = false;
   turn = 17; 
   last_move = (3,2);
   prev_st = None   
  }

let win_on_turn_42_col =
  {boardGame = [
      [1;2;1;2;2;2];
      [1;2;1;1;1;1];
      [2;1;2;2;2;1];
      [2;1;2;1;1;2];
      [1;2;1;2;2;2];
      [1;2;2;1;1;1];
      [2;1;1;1;2;2]
    ];colDepth = [6;6;6;6;6;6];
   player1 = false; turn =42; last_move = (5,1);
   prev_st = None
  }

(* let make_update_board_test
    (name : string)
    (st : State.t)
    (i : int)
    (expected_output : State.t) : test =
   name >:: (fun _ ->
      assert_equal expected_output (State.update_board st i)) *)

let make_update_colDepth_test
    (name : string)
    (colDepth : int list)
    (col : int)
    (expected_output : int list) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (State.update_colDepth colDepth col))

let make_update_colDepth_raises_test
    (name : string)
    (colDepth : int list)
    (col : int)
    (expected_output : exn) : test =
  name >:: (fun _ -> 
      assert_raises expected_output 
        (fun () -> State.update_colDepth colDepth col))

let make_update_lastmove_test
    (name : string)
    (colDepth : int list)
    (col : int)
    (expected_output : int*int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (State.update_last_move colDepth col))

(* player2 wins *)
let state3 =
  {boardGame = 
     [[0; 0; 0; 0; 0; 0]; 
      [2; 0; 0; 0; 0; 0]; 
      [1; 1; 2; 0; 0; 0];
      [1; 2; 1; 0; 0; 0]; 
      [1; 1; 2; 1; 0; 0]; 
      [2; 2; 2; 2; 0; 0];
      [2; 0; 0; 0; 0; 0]];
   colDepth = [0; 1; 3; 3; 3; 4; 1];
   player1 = true; 
   turn = 15; 
   last_move = (3, 5);
   prev_st = None   
  }
(* Making the Test functions*)
(* State Test *)
let make_update_board_test
    (name : string)
    (st : State.t)
    (i : int)
    (expected_output : int list list) : test =
  name >:: (fun _ ->
      assert_equal (State.update_board st i) expected_output)

let example_state =
  {boardGame = 
     [[0; 0; 0; 0; 0; 0]; 
      [2; 0; 0; 0; 0; 0]; 
      [1; 1; 2; 0; 0; 0];
      [1; 2; 1; 0; 0; 0]; 
      [1; 1; 2; 0; 0; 0]; 
      [2; 0; 0; 0; 0; 0];
      [2; 0; 0; 0; 0; 0]];
   colDepth = [0; 1; 3; 3; 3; 1;1];
   player1 = true; 
   turn = 12; 
   last_move = (6, 0);
   prev_st = None   
  }

let make_winner_test
    (name : string)
    (st : State.t)
    (expected_output : int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (winner st))

let make_update_state_test
    (name: string)
    (st: t)
    (col: int)
    (expected_output : State.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (State.update_state st col) )

let state_tests =
  [
    make_update_colDepth_test "example_state 0" example_state.colDepth 0 
      [1; 1; 3; 3; 3; 1;1];
    make_update_colDepth_test "example_state 1" example_state.colDepth 1 
      [0; 2; 3; 3; 3; 1;1];
    make_update_colDepth_test "example_state 2" example_state.colDepth 2 
      [0; 1; 4; 3; 3; 1;1];
    make_update_colDepth_test "example_state 3" example_state.colDepth 3
      [0; 1; 3; 4; 3; 1;1];
    make_update_colDepth_test "example_state 4" example_state.colDepth 4 
      [0; 1; 3; 3; 4; 1;1];
    make_update_colDepth_test "example_state 5" example_state.colDepth 5 
      [0; 1; 3; 3; 3; 2; 1];
    make_update_colDepth_test "example_state 6" example_state.colDepth 6 
      [0; 1; 3; 3; 3; 1; 2];
    (*make_update_colDepth_raises_test "example_state full" [6;6;6;6;6;6] 0 
      State.ColumnFull;*)


    make_update_board_test "init to zeroth col" State.init_state 0 
      [[1; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]];

    make_update_board_test "init to first col" State.init_state 1 
      [[0; 0; 0; 0; 0; 0]; [1; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]];

    make_update_board_test "init to second col" State.init_state 2 
      [[0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [1; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]];

    make_update_board_test "init to third col" State.init_state 3 
      [[0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [1; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]];

    make_update_board_test "init to fourth col" State.init_state 4 
      [[0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [1; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]]; 

    make_update_board_test "init to fifth col" State.init_state 5 
      [[0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [1; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]];

    make_update_board_test "init to sixth col" State.init_state 6 
      [[0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [1; 0; 0; 0; 0; 0]]; 

    make_update_board_test "second turn, on top of another coin" examp 0 
      [[1; 2; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]];

    make_update_board_test "second turn, next to another coin" examp 1 
      [[1; 0; 0; 0; 0; 0]; [2; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0];
       [0; 0; 0; 0; 0; 0]];


    make_winner_test "init test" initial_state 0;
    make_winner_test "no one wins" state1 0;
    make_winner_test "player 1 wins" state2 1; 
    make_winner_test "player 2 wins" state3 2;
    make_winner_test "row win" row_win 0;
    make_winner_test "row win 2" row_win_2 0; 
    make_winner_test "player 1 wins left diagonal" player1winsLD 1;
    make_winner_test "right diagonal" right_diag 2; 
    make_winner_test "right diagonal 2" right_diag2 2;
    

    make_update_lastmove_test "example 0" example_state.colDepth 0 (0,0);
    make_update_lastmove_test "example 1" example_state.colDepth 1 (1,1);
    make_update_lastmove_test "example 2" example_state.colDepth 2 (3,2);
    make_update_lastmove_test "example 3" example_state.colDepth 3 (3,3);
    make_update_lastmove_test "example 4" example_state.colDepth 4 (3,4);
    make_update_lastmove_test "example 5" example_state.colDepth 5 (1,5);
    make_update_lastmove_test "example 6" example_state.colDepth 6 (1,6); 

    make_update_state_test "init to examp" init_state 0 examp;
    make_update_state_test "examp to examp2" examp 1 examp2;
    make_update_state_test "examp2 to examp3" examp2 0 examp3;  
  ]

let suite =
  "test suite for A6" >::: List.flatten [
    state_tests;
  ]

let _ = run_test_tt_main suite