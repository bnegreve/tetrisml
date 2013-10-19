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
};;

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
    color = black; };;

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
    color = green; };;

let make_rs_shaped_piece =
  { pos = start_pos;
    blocks = {x = 0; y = 0}::{x = 0; y = -1}::{x = -1; y = 0}::{x = -1; y = 1}::[]; 
    color = green; };;

let make_line_shaped_piece =
  { pos = start_pos;
    blocks = {x = -1; y = 0}::{x = 0; y = 0}::{x = 1; y = 0}::{x = 2; y = 0}::[]; 
    color = green; };;

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
    
let rotate_piece world piece = 
  let rotated_piece ={piece with blocks = 
      List.map (function block -> { x = block.y; y = -block.x}) piece.blocks} in 
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

let drop_new_piece world = {world with 
  current_piece = make_a_piece (Random.int 7)}
    
let drop_current_piece world =
  let translated_piece = translate_piece_y world.current_piece 1 in
  if not (piece_collides world translated_piece) then 
    {world with current_piece = translated_piece}
  else
    drop_new_piece (stack_piece world world.current_piece);;

(*** keyboard functions ***)

let update_world_with_input world () =
  let event = Graphics.wait_next_event [ Graphics.Poll ] in
  if event.Graphics.keypressed then
    match (read_key ()) with
     'd'      -> set_redraw (move_piece_right world world.current_piece)
    |'a'|'q'  -> set_redraw (move_piece_left world world.current_piece)
    |'s'      -> set_redraw (rotate_piece world world.current_piece)
    | x       -> world
  else
    world;;

let update_world world =
  let new_world = (update_world_with_input world ()) in
  new_world;;

(*** Timing functions ***) 

let get_time_now () = 
  Unix.gettimeofday ();;

(* Wait until the global clock reaches time (in s from beginning of
   Unix time) *)
let rec wait_until time =
  let now = get_time_now () in 
  let wait_time = (time -. now) in
  (* fprintf stderr "WAIT TIME %f sec.\n" wait_time; *)
  if wait_time > 0.000125 then
    try
      Thread.delay wait_time
    with Unix.Unix_error (Unix.EAGAIN, _, _) -> wait_until time 
  else
   fprintf stderr "Warning: frame delayed (time diff: %f sec.)\n" wait_time;;


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
  draw_block_list (List.map (fun block -> get_absolute_coords piece.pos block) 
		     piece.blocks);;

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

let draw_world world =
  if world.redraw then 
    (clear_graph ();
     draw_frame ();
     draw_block_matrix world.block_matrix;
     draw_piece world.current_piece;
     {world with redraw = false})
  else 
    world;; 


(*** Main functions ***)

let create_world () = 
  Random.self_init ();
  open_graph " ";
  resize_window screen_width screen_height;
  {
    current_piece = make_a_piece (Random.int 7);
    block_matrix = ref (Array.make_matrix area_width area_height 0);
    lap_start = get_time_now ();
    redraw = true;
  };;

let start_lap world = 
  world;;

let reset_lap_start world = 
  {world with lap_start = get_time_now ()};;

let finilize_lap world =
  let now = get_time_now () in  
  if ( now >= world.lap_start +. lap_length) then
    reset_lap_start (set_redraw (drop_current_piece world))
  else
    world;;

let rec run world = 
  run (draw_world (finilize_lap (update_world (start_lap world))));;

let main = 
  run (create_world ())
