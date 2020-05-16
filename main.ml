open State
open Unix
open AI

(** [win st] returns 1 if player 1 has won; 2 if player 2 (or AI) has won; 
    othwerwise, returns 0. *)
let win st : int=
  State.winner st

(** [print_token input] prints a token of color [input]. The token blinks
    if it represents the most recent move. *)
let print_token input = 
  ANSITerminal.(print_string input ("o"))

(** [empty_space ()] prints an empty space, since there is no player in 
    that space *)
let empty_space () = 
  ANSITerminal.(print_string [cyan] (" "))

(** [pp_help2 st i c1 c2 col row] prints the token of player [i] with their 
    chosen color. If there is not a player in the coordinate, it will print a
    whitespace. Also, the most recent token that is added to the game board, 
    which is determined by [st], [col], and [row], is made to blink. 
    Examples: 
    [pp_help2 1 ANSITerminal.red ANSITerminal.cyan] will print a red o
    [pp_help2 0 ANSITerminal.red ANSITerminal.cyan] will print a space. *)
let pp_help2 st i c1 c2 (col: bool) row =
  if col && fst st.last_move = row 
  then 
    if i=1 then print_token [c1; Blink] 
    else if i=2 then print_token [c2; Blink] 
    else empty_space ()
  else 
  if i=1 then print_token [c1]
  else if i=2 then print_token [c2]
  else empty_space ()

(** [printrow st i c1 c2 col last_c] recursively prints every token in row [i] 
    in state [st], starting with column [col] being 0. If a token is in the 
    greatest row and in the column [last_c], then that token is made to blink.
    It prints player 1's tokens using color [c1] and player 2's tokens using 
    color [c2]. *)
let rec printrow st i c1 c2 col last_c =
  let p () = ANSITerminal.(print_string [default] " | ") in
  if col<6 then 
    (pp_help2 st (List.nth (List.nth st.boardGame col) i) 
       c1 c2 (if last_c=col then true else false) i; p();
     printrow st i c1 c2 (col+1) last_c)
  else 
    (pp_help2 st (List.nth (List.nth st.boardGame col) i) 
       c1 c2 (if last_c=col then true else false) i)

(** [pp_help st i c1 c2] prints the row [i] from the boardGame in [st], then
    recursively calls itself to print the row below it.  
    It prints player 1's tokens using color [c1] and player 2's tokens using 
    color [c2].
    Example: [pp_help st 4 ANSITerminal.red ANSITerminal.cyan] could print
      | o | o | o | o |   | o |   |
       ----------------------------   *)
let rec pp_help st i c1 c2 =
  ANSITerminal.(print_string [default] " "); 
  ANSITerminal.(print_int i); 
  ANSITerminal.(print_string [default] "    | ");
  printrow st i c1 c2 0 (snd st.last_move);
  ANSITerminal.(print_string [default] " | \n");
  ANSITerminal.(print_endline "       ----------------------------");
  if i=0 then () else pp_help st (i-1) c1 c2 

(** [pretty_printing st c1 c2] is a helper function that prints out the current 
    configuration of the board, with [c1] bring player 1's chosen color, and 
    [c2] being player 2's chosen color
    Example:

        0   1   2   3   4   5   6

       ----------------------------
    5    | o | o |   |   |   | o |   |
       ----------------------------
    4    | o | o |   |   |   | o |   |
       ----------------------------
    3    | o | o | x | x |   | o |   |
       ----------------------------
    2    | o | o | x | x |   | o |   |
       ----------------------------
    1    | o | o | x | x |   | o |   |
       ----------------------------
    0    | o | o | x | x |   | o |   |
       ----------------------------         *)
let pretty_printing st c1 c2 =
  print_endline "";
  print_endline "        0   1   2   3   4   5   6";
  print_endline " ";
  print_endline "       ----------------------------";
  pp_help st 5 c1 c2 

(** [is_valid_col] returns true if [i] is a valid column number, 
    false otherwise. "Valid" is defined as within the game board
    (0 to 6, inclusive) and not a full column
    Example: [is_valid_col 10 st] returns false
            [is_valid_col 1 init_state] returns true*)
let is_valid_col i st =
  i>(-1) && i< 7 && (List.nth st.colDepth i <> 6)

(** [winning st n1 n2] is a winning message that declares whether player [n1]  
    or player [n2] has won, which is determined by the state [st]. *)
let winning st n1 n2 = 
  (print_endline ("Congrats! "^(if st.player1 then n2 else n1)^" just won!");
   exit(0))

(** [playing st n1 n2 c1 c2 back1 back2 mode] is a incessent loop where the game  
    is played by alternating each player's turn. If either player wins or 42 
    turns have been made and no one has won, the game terminates. [back1] and 
    [back2] represent [n1]'s and player [n2]'s abilities, respectively, to 
    go back one move. *)
let rec playing st n1 n2 c1 c2 back1 back2 mode =
  let end_game = win st<>0 in
  if end_game then pretty_printing st c1 c2 
  else pretty_printing st c1 c2;
  if (end_game) then 
    winning st n1 n2
  else if (State.get_turn st = 42) then 
    (print_endline ("DRAW: Yikes! No one won! The game has ended.");
     exit(0))
  else print_endline 
      ("\n\n\n"^(if st.player1 then n1 else n2)^", please insert the"^ 
       " column number you would like to drop your coin into" ^
       ", or type 'back' if you would like to go back 1 move"^
       "\n\n\n\n\n\n\n\n\n\n\n\n");
  match read_line () with
  | instruction -> try (
    let col = int_of_string instruction in
    if is_valid_col col st then 
      (ANSITerminal.erase(Above);
       if (mode > 0) 
       then playingAI (State.update_state st col) n1 n2 c1 c2 back1 back2 mode
       else playing (State.update_state st col) n1 n2 c1 c2 back1 back2 mode)
    else print_endline "";
    ANSITerminal.(print_string [red]
                    ((if st.player1 then n1 else n2)^
                     ": you must place your piece in a valid column!"));
    playing st n1 n2 c1 c2 back1 back2 mode) with ex -> 
    if instruction = "back" && st.turn >= 2 && 
       (if st.player1 then back1 else back2) then 
      match st.prev_st with 
      |Some m -> (match m.prev_st with 
          |Some k -> (if st.player1 then playing k n1 n2 c1 c2 false back2 mode
                      else playing k n1 n2 c1 c2 back1 false mode)
          |None -> failwith "The previous state must exist after 2+ turns.")
      | None -> failwith "The previous state must exist after 2+ turns."
    else 
    if String.lowercase_ascii (String.trim instruction) = "quit" then exit(0) 
    else
      (print_endline "This input is invalid. Please try again!";
       playing st n1 n2 c1 c2 back1 back2 mode)

and 

  (** [playingAI st n1 n2 c1 c2 back1 back2 mode] represents the AI's move. 
      It selects an optimal column and moves there. *)
  playingAI st n1 n2 c1 c2 back1 back2 mode =
  let end_game = win st<>0 in
  if end_game then pretty_printing st c1 c2
  else pretty_printing st c1 c2;
  if (end_game) then 
    winning st n1 n2
  else if (State.get_turn st = 42) then 
    (print_endline ("DRAW: Yikes! No one won! The game has ended.");
     exit(0))
  else (
    let thinking_message = 
      if mode = 1 then "NingNing is thinking . . . like a bosss :) " 
      else if mode = 2 then "Foster is thinking . . ."
      else "David Gries is thinking . . . (but not too hard)" in 
    ANSITerminal.(print_string [yellow; Bold]  
                    ("\n\n\n"^thinking_message^"\n");
                  print_endline "\n\n\n\n\n\n\n\n\n\n\n"; Unix.sleep(2); 
                  print_endline ""; print_endline ""; print_endline ""; 
                  print_endline ""; print_endline ""; print_endline ""; 
                  print_endline ""; ANSITerminal.erase(Above));      );
  let col = ai_generated_col st mode in
  playing (State.update_state st col) n1 n2 c1 c2 back1 back2 mode

(** [print_colors c i] prints all colors (strings) in list [c] 
    in a numbered list starting with the number [i].
    Example: 
    colors = ["red"; "yellow"; "blue"]
    [print_colors colors 1] prints:
    1 red
    2 yellow
    3 blue *)
let rec print_colors c i =
  match c with
  |[] -> print_endline ""
  | h::t -> print_endline ("  "^(string_of_int i)^" "^h); 
    print_colors t (i+1)

(* a list of colors available represented by strings *)
let colors = ["red"; "yellow"; "green";
              "cyan"; "blue"; "magenta"]  

(** [get_colors s] is the ANSITerminal color type that string [s] represents.
    A helper function for [main]
    Example: [get_colors "yellow"] returns ANSITerminal.yellow*)
let rec get_colors s =
  match s with
  |"red" -> ANSITerminal.red
  |"yellow" -> ANSITerminal.yellow
  |"green" -> ANSITerminal.green
  |"cyan" -> ANSITerminal.cyan
  |"blue" -> ANSITerminal.blue
  |"magenta" -> ANSITerminal.magenta
  |_ -> ANSITerminal.default (* will never get here*)

(** [choose_mode ()] returns true if 2-player mode is selected. It 
    returns false if the AI-mode is selected. *)
let rec choose_mode () : bool = 
  print_endline ("Would you like to do 2 player or 1 player (versing the AI)?"^
                 "\nEnter 1 or 2.");
  print_string "> ";
  match read_line () with 
  | "2" -> true
  | "1" -> false
  | _ ->  (print_endline "That was not a valid choice. Please choose again."; 
           choose_mode ())

(** [choose_AI_diff ()] allows the user to choose the level of difficulty. 
    The player can input 1, 2, or 3 to pick easy, medium, or hard, 
    respectively. *)
let rec choose_AI_diff () = 
  print_endline 
    "Which AI difficulty? (NingNing = easy, Foster = medium, Gries = hard)";
  print_string "> ";
  match read_line () with 
  | "NingNing" -> 1
  | "Foster" -> 2
  | "Gries" -> print_endline  
                 ("\n\nI, David Gries, have 56 years of connect 4 experience."^ 
                  "You have maybe one, I think I will win. "); 3
  | _ ->  (print_endline "That was not a valid choice. Please choose again."; 
           choose_AI_diff ())

(** [get_name () s] gets name of player [s].
    Example: [get_name () 1] would print:
    "Player one, what is your name?"
    and could be: "Megan" *)
let rec get_name () s =
  print_endline ("Player "^s^", what is your name?"); print_string  "> ";
  match read_line () with
  | name -> name

(** [get_color_choice n c] is the color that the player named [n] chose. 
    It is a tuple with the first element containing a string representation 
    of the color, and the second element containing the ANSITerminal.style 
    representation of the color
    Example:
    get_color_choice () "Emily" ["red", "yellow", "blue"] could be:
    ("red", ANSITerminal.red) *)
let rec get_color_choice () n c : (string*ANSITerminal.style) =
  print_endline ""; print_endline "";
  print_endline (n^", please select a color by typing its number");
  print_endline "";
  print_endline "Your options are:"; print_endline "";
  print_colors c 1;
  print_string "> ";
  match read_line () with
  | line -> 
    (try 
       print_endline "";

       let c1 = List.nth c (int_of_string line - 1) in
       let c1_a = get_colors c1 in
       print_endline "";
       print_string  "Color Chosen: ";
       ANSITerminal.print_string [c1_a] c1;
       print_endline "";
       print_endline ""; print_endline "";
       (line, c1_a)
     with ex -> 
       (print_endline ("Invalid Color! Please try again"); 
        get_color_choice () n c))

(** [nc l c] is the list of colors [l] without the color [c] that the first 
    player already chose. *)
let rec nc l c = 
  match l with
  |[] -> []
  | h::t -> if (c=h) then nc t c else h::(nc t c)

(** [get_color_choice_ai new1] is a randomly chosen ANSITerminal color 
    that player 1 did not pick. *)
let rec get_color_choice_ai new1 = 
  get_colors (List.nth new1 ((int_of_float (Sys.time ()) mod 7)))

(** [main st] is the main function for the game, which initiaiates the game 
    engine. *)
let rec main () = 
  (* play init_state *)
  ANSITerminal.(print_string [green]
                  ("\n\nWelcome to the Connect 4 game! Make sure your"^
                   " terminal is in full screen for an optimal "^
                   "playing experience :) \n"));
  (* if player selected one-player, proceed with AI *)
  if (choose_mode () = false) then 
    let c1 = get_color_choice () "human" colors in
    let new_colors = nc colors (List.nth colors (int_of_string (fst c1) - 1 )) 
    in
    let c2 = get_color_choice_ai new_colors in
    playing init_state "Human" "AI" (snd c1) c2 
      true false (choose_AI_diff ())
      (* otherwise, player selected two player mode *)
  else  
    let name1 = get_name () "one" in
    let c1 = get_color_choice () name1 colors in
    let name2 = get_name () "two" in
    let new_colors = nc colors (List.nth colors (int_of_string (fst c1) - 1)) 
    in
    let c2 = get_color_choice () name2 new_colors in
    print_endline ""; print_endline "";
    ANSITerminal.print_string [Blink] "Let's Begin!";
    print_endline ""; Unix.sleep 2;
    playing init_state name1 name2 (snd c1) (snd c2) true true (-1)

let () = main ()