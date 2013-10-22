(*
*  test.ml
*
*  Made by (Benjamin Negrevergne)
*  Login   <benjamin@neb.dyn.cs.kuleuven.be>
*
*  Started on  Sat Oct 12 14:23:30 2013 Benjamin Negrevergne
*)

open Graphics;;
open Printf;;

let area_width = 10;; (* playable area width, in # blocks *)
let area_height = 18;; 

let left_edge = 1;; 
let top_edge = 1;;
let right_edge = left_edge + area_width - 1;; 
let bottom_edge = top_edge + area_height - 1;;

let block_size = 10;;
let block_padding = 2;;
let screen_width = block_size * 12;;
let screen_height = block_size * 20;;

let lap_length = 0.5;; (* in sec *)

let game_over_initial_count = 5; 

type block_pos = {x: int; y: int};;
type pixel_pos = {x_pixel: int; y_pixel: int};;
type piece = 
  { pos: block_pos; blocks: block_pos list; color: int};;
let start_pos = { x = (right_edge - left_edge) / 2; 
		  y = top_edge + 1 };;

type collision = NO_COLLISION|LEFT|RIGHT|UP|DOWN|ANY;;

type world = {
  current_piece: piece;
  block_matrix: int array array ref;
  lap_start: float; 
  redraw: bool;
  line_count: int; 
  score: int; 
  game_over_count: int; 
  filling_height: int;
};;

(*** Score ***)

let score_function num_line = 
  match num_line with 
    4 -> 1000
  | n -> n * 10;; 

(*** Utility functions ***)

let get_absolute_coords origin point = 
  {x = (origin.x + point.x); y = (origin.y + point.y)};;

let print_pos pos = 
  Printf.printf "Pos: [%d, %d]\n" pos.x pos.y;;

let print_world world = 
  Printf.printf "lap_start %f, redraw %B\n" world.lap_start world.redraw; 
world;;

let set_redraw world = 
  { world with redraw = true };;

(*** Pieces description ***)

let make_square_shaped_piece =
  { pos = start_pos;
    blocks = {x = 0; y = 0}::{x = 1; y = 0}::{x = 1; y = 1}::{x = 0; y = 1}::[]; 
    color = red; };;

let make_l_shaped_piece =
  { pos = start_pos;
    blocks = {x = 0; y = -1}::{x = 0; y = 0}::{x = 0; y = 1}::{x = 1; y = 1}::[]; 
    color = magenta; };;

let make_rl_shaped_piece =
  { pos = start_pos;
    blocks = {x = 0; y = -1}::{x = 0; y = 0}::{x = 0; y = 1}::{x = -1; y = 1}::[]; 
    color = blue; };;

let make_t_shaped_piece =
  { pos = start_pos;
    blocks = {x = 0; y = 0}::{x = -1; y = 0}::{x = 1; y = 0}::{x = 0; y = 1}::[]; 
    color = green; };;

let make_s_shaped_piece =
  { pos = start_pos;
    blocks = {x = 0; y = 0}::{x = 0; y = -1}::{x = 1; y = 0}::{x = 1; y = 1}::[]; 
    color = cyan; };;

let make_rs_shaped_piece =
  { pos = start_pos;
    blocks = {x = 0; y = 0}::{x = 0; y = -1}::{x = -1; y = 0}::{x = -1; y = 1}::[]; 
    color = magenta; };;

let make_line_shaped_piece =
  { pos = start_pos;
    blocks = {x = -1; y = 0}::{x = 0; y = 0}::{x = 1; y = 0}::{x = 2; y = 0}::[]; 
    color = red; };;

let make_single_block_piece =
  { pos = start_pos;
    blocks = {x = 0; y = 0}::[]; 
    color = black; };;

let make_a_piece pieceid =
  match pieceid with 
    0 -> make_l_shaped_piece
  | 1 -> make_rl_shaped_piece 
  | 2 -> make_square_shaped_piece
  | 3 -> make_s_shaped_piece
  | 4 -> make_rs_shaped_piece
  | 5 -> make_t_shaped_piece
  | 6 -> make_line_shaped_piece
  | x -> make_l_shaped_piece;;

(* Collision detection *)

let block_collides world block_pos = 
  if block_pos.x < left_edge || block_pos.x > right_edge ||
    block_pos.y > bottom_edge || block_pos.y < top_edge then 
    true (* anything outside of the playable area is conceptually filled with blocks. *)
  else
    !(world.block_matrix).(block_pos.x - left_edge).(block_pos.y - top_edge) != 0;;

let rec block_list_collides world block_list = 
  match block_list with 
    [] -> false
  | head :: tail -> block_collides world head || block_list_collides world tail;;

let piece_collides world piece =
  let block_list =
    List.map (function block -> {x = piece.pos.x + block.x ; y = piece.pos.y + block.y})
      piece.blocks in
  block_list_collides world block_list;;

(*** Move pieces ***)

let translate_piece_x piece x = 
  { piece with pos =
    { piece.pos with x = piece.pos.x + x }}

let translate_piece_y piece y = 
  { piece with pos =
    { piece.pos with y = piece.pos.y + y }}

let move_piece_right world piece =
  let translated_piece = translate_piece_x world.current_piece 1 in
    if not (piece_collides world translated_piece) then 
      {world with current_piece = translated_piece}
  else 
      world;;

let move_piece_left world piece =
  let translated_piece = translate_piece_x world.current_piece (-1) in
    if not (piece_collides world translated_piece) then 
      {world with current_piece = translated_piece}
    else 
      world;;

let rotate_piece_cw piece =
  {piece with blocks = 
      List.map (function block -> { x = -block.y; y = block.x}) piece.blocks};;

let rotate_piece_ccw piece =
  {piece with blocks = 
      List.map (function block -> { x = block.y; y = -block.x}) piece.blocks};;

let rotate_the_piece_cw world piece = 
  let rotated_piece = rotate_piece_cw world.current_piece in 
  if not (piece_collides world rotated_piece) then 
    {world with current_piece = rotated_piece} else 
    world;;

let rotate_the_piece_ccw world piece = 
  let rotated_piece = rotate_piece_ccw world.current_piece in 
  if not (piece_collides world rotated_piece) then 
    {world with current_piece = rotated_piece} else 
    world;;

let stack_block world block = 
  !(world.block_matrix).(block.x - left_edge).(block.y - top_edge) <- 1; 
  world;;
    
let rec stack_blocks world block_list =
  match block_list with 
    [] -> world
  | head :: tail -> stack_blocks (stack_block world head) tail;;

let stack_piece world piece =
  let piece_blocks_list = 
    List.map (fun block -> get_absolute_coords piece.pos block) 
      piece.blocks in
  stack_blocks world piece_blocks_list

let drop_new_piece world =
  let new_piece = make_a_piece (Random.int 7) in 
    if piece_collides world new_piece then 
      { world with
	game_over_count = world.game_over_count - 1;
	current_piece = new_piece }
    else
      {world with current_piece = new_piece }
    
let drop_current_piece world =
  let translated_piece = translate_piece_y world.current_piece 1 in
  if not (piece_collides world translated_piece) then 
    {world with current_piece = translated_piece}
  else
    drop_new_piece (stack_piece world world.current_piece);;

let rec is_a_line world line_id x =
  if x == area_width then true 
  else if !(world.block_matrix).(x).(line_id) == 0 then false
  else is_a_line world line_id (x+1);;

let rec find_all_lines world y =
  if y == area_height then [] else
    if is_a_line world y 0 then 
      y :: (find_all_lines world (y + 1))
    else find_all_lines world (y + 1);;

let rec push_blocks_down world line_id =
  let matrix = world.block_matrix in
  if line_id > 0 then (
    for i = 0 to (area_width-1) do
      !matrix.(i).(line_id) <- !matrix.(i).(line_id-1)
    done; push_blocks_down world (line_id - 1))
 else ();;

let remove_lines world = 
  let all_lines = find_all_lines world 0 in
  ignore (List.map (fun line_id -> push_blocks_down world line_id) all_lines);
  {world with 
    line_count = world.line_count + (List.length all_lines);
    score = world.score + score_function (List.length all_lines)
  } ;;

let fill_line world line_id =
  let matrix = world.block_matrix in
  for i = 0 to (area_width-1) do
    !matrix.(i).(line_id) <- 1
  done; ();; 

(*** keyboard functions ***)

let update_world_with_input world =
  let event = Graphics.wait_next_event [ Graphics.Poll ] in
  if event.Graphics.keypressed then
    match (read_key ()) with
     'd'      -> set_redraw (move_piece_right world world.current_piece)
    |'a'|'q'  -> set_redraw (move_piece_left world world.current_piece)
    |'z'|'w'  -> set_redraw (rotate_the_piece_cw world world.current_piece)
    |'s'      -> set_redraw (rotate_the_piece_ccw world world.current_piece)
    | x       -> world
  else
    world;;

(*** Timing functions ***) 

let get_time_now () = 
  Unix.gettimeofday ();;

let reset_lap_start_timer world = 
  {world with lap_start = get_time_now ()};;

(*** Drawing functions ***)

let get_pixel_coords pos = 
  { x_pixel = (pos.x * block_size);
    y_pixel = (screen_height - block_size) - (pos.y * block_size); };;
  
let draw_block_with_pixel_coords x y = 
  (* Printf.printf "Pos: [%d, %d]\n" x y; *)
  draw_rect x y block_size block_size; 
  fill_rect (x + block_padding) (y + block_padding) 
    (block_size - (2 * block_padding)) (block_size - (2 * block_padding));;

let draw_block pos =
  (* print_pos pos; *)
  let pixel_pos = get_pixel_coords pos in 
  draw_block_with_pixel_coords pixel_pos.x_pixel pixel_pos.y_pixel;;

let rec draw_block_list blocks =
  List.map draw_block blocks;;

let draw_piece piece  = 
  set_color piece.color; 
  ignore (draw_block_list (List.map (fun block -> get_absolute_coords piece.pos block) 
		     piece.blocks));;

let draw_frame () = 
  set_color black;
  let a = get_pixel_coords({x = left_edge; y = top_edge - 1}) in 
  let b = get_pixel_coords({x = left_edge; y = bottom_edge}) in 
  let c = get_pixel_coords({x = right_edge + 1; y = bottom_edge}) in 
  let d = get_pixel_coords({x = right_edge + 1; y = top_edge - 1}) in 
  draw_poly [| (a.x_pixel , a.y_pixel) ; 
	       (b.x_pixel , b.y_pixel) ; 
	       (c.x_pixel , c.y_pixel) ; 
	       (d.x_pixel , d.y_pixel) |];;

let draw_block_matrix block_matrix = 
  for i = 0 to area_width - 1 do 
    for j = 0 to area_height - 1 do 
      if (!block_matrix.(i).(j)) != 0 then 
	draw_block { x = i + left_edge; y = j + top_edge}
    done
  done; ();;

let draw_score world =
  set_color black;
  let text_pos = get_pixel_coords ({x = left_edge; y = bottom_edge + 1}) in 
  moveto text_pos.x_pixel (text_pos.y_pixel - 2);
  draw_string (Printf.sprintf "l. %d" world.line_count);
  let text_pos = 
    get_pixel_coords ({x = (right_edge+4 - left_edge) / 2; y = bottom_edge + 1}) in 
  moveto text_pos.x_pixel (text_pos.y_pixel - 2);
  draw_string (Printf.sprintf "s. %d" world.score);
  ();;

let draw_world world =
  if world.redraw then 
    (clear_graph ();
     draw_frame ();
     draw_block_matrix world.block_matrix;
     draw_piece world.current_piece;
     draw_score world;
     synchronize (); 
     {world with redraw = false})
  else 
    world;; 


(*** Main functions ***)

let init () = 
  Random.self_init ();
  open_graph " ";
  set_window_title "tetrisML";
  resize_window screen_width screen_height;
  auto_synchronize false;;

let create_world () = {
    current_piece = make_a_piece (Random.int 7);
    block_matrix = ref (Array.make_matrix area_width area_height 0);
    lap_start = get_time_now ();
    redraw = true;
    line_count = 0; 
    score = 0; 
    game_over_count = game_over_initial_count; 
    filling_height = area_height - 1; 
  };;

let update_world world =
  update_world_with_input world;;

let game_over world =
  if world.filling_height >= 0 then
    (fill_line world world.filling_height; 
     set_redraw (reset_lap_start_timer (
       { world with filling_height = world.filling_height - 1})))
  else
    create_world ();;

let finilize_lap world =
  let now = get_time_now () in  
  if ( now >= world.lap_start +. lap_length) then
    if world.game_over_count == 0 then 
      game_over world
    else 
      reset_lap_start_timer (set_redraw (remove_lines (drop_current_piece world)))
  else
    world;;

let rec run world = 
  run (draw_world (finilize_lap (update_world world)));;

let main = 
  init (); 
  run (create_world ())
