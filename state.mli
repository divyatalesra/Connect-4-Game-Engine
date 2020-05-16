type t = {
  boardGame : int list list;
  colDepth: int list;
  player1:bool;
  turn:int;
  last_move: int*int;
  prev_st: t option 
}

val init_state : t

val get_cell : t -> int -> int -> int

val update_board: t -> int -> int list list

val update_colDepth: int list -> int -> int list

val update_last_move: int list -> int -> int*int 

val update_state : t -> int -> t 

val only_change_player : t -> t

val winner : t -> int

val get_turn : t -> int
