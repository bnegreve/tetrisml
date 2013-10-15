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

let block_size = 10;;
let block_padding = 2;;
let screen_width = block_size * 12;;
let screen_height = block_size * 20;;
let lap_length = 1.;; (* in sec *)
let left_edge = 1;; 
let right_edge = 10;; 
let bottom_edge = 18;;
let h_center = (right_edge - left_edge) / 2;;

type block_pos = {x: int; y: int};;
type pixel_pos = {x_pixel: int; y_pixel: int};;
type piece = 
  { pos: block_pos; blocks: block_pos list; color: int};;
type collision = NO_COLLISION|LEFT|RIGHT|UP|DOWN;;

type world = {
  current_piece: piece;
  stacked_blocks: block_pos list;
  lap_start: float; 
  redraw: bool; 
};;

let set_redraw world = 
  { world with redraw = true };;

let get_absolute_coords origin point = 
  {x = (origin.x + point.x); y = (origin.y + point.y)};;

let print_pos pos = 
  Printf.printf "Pos: [%d, %d]\n" pos.x pos.y;;

let print_world world = 
  Printf.printf "lap_start %f, redraw %B\n" world.lap_start world.redraw; 
world;;

let make_square_shaped_piece =
  { pos = {x = h_center; y = 0};
    blocks = {x = 0; y = 0}::{x = 1; y = 0}::{x = 1; y = 1}::{x = 0; y = 1}::[]; 
    color = red; };;

let make_l_shaped_piece =
  { pos = {x = h_center; y = 0};
    blocks = {x = 0; y = -1}::{x = 0; y = 0}::{x = 0; y = 1}::{x = 1; y = 1}::[]; 
    color = black; };;

let make_rl_shaped_piece =
  { pos = {x = h_center; y = 0};
    blocks = {x = 0; y = -1}::{x = 0; y = 0}::{x = 0; y = 1}::{x = -1; y = 1}::[]; 
    color = blue; };;

let make_t_shaped_piece =
  { pos = {x = h_center; y = 0};
    blocks = {x = 0; y = 0}::{x = -1; y = 0}::{x = 1; y = 0}::{x = 0; y = 1}::[]; 
    color = green; };;

let make_s_shaped_piece =
  { pos = {x = h_center; y = 0};
    blocks = {x = 0; y = 0}::{x = 0; y = -1}::{x = 1; y = 0}::{x = 1; y = 1}::[]; 
    color = green; };;

let make_rs_shaped_piece =
  { pos = {x = h_center; y = 0};
    blocks = {x = 0; y = 0}::{x = 0; y = -1}::{x = -1; y = 0}::{x = -1; y = 1}::[]; 
    color = green; };;

let make_line_shaped_piece =
  { pos = {x = h_center; y = 0};
    blocks = {x = 0; y = 0}::{x = 0; y = -2}::{x = 0; y = -1}::{x = 0; y = 1}::[]; 
    color = green; };;

let make_single_block_piece =
  { pos = {x = h_center; y = 0};
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

let rotate_piece piece = 
  {piece with blocks = 
      List.map (function block -> { x = block.y; y = -block.x}) piece.blocks};;

let check_collision_with_other_blocks block direction = 
  NO_COLLISION;;

let rec check_edge_collision block_list direction =
  match block_list with 
    [] -> NO_COLLISION
  | head :: tail -> 
    match direction with 
      LEFT -> if head.x < left_edge then LEFT
	else check_edge_collision tail direction 
    | RIGHT -> if head.x > right_edge then RIGHT
      else check_edge_collision tail direction 
    | DOWN -> if head.y > bottom_edge then DOWN
      else check_edge_collision tail direction 
    | default -> NO_COLLISION;;

let check_piece_collision piece direction =
  let block_list =
    List.map (function block -> {x = piece.pos.x + block.x ; y = piece.pos.y + block.y})
      piece.blocks in 
  check_edge_collision block_list direction;;

let stack_blocks stacked_block_list blocks =
  List.fold_left (fun acc x -> x :: acc) stacked_block_list blocks;;

let stack_piece stacked_blocks piece =
  let piece_blocks_list = 
    List.map (fun block -> get_absolute_coords piece.pos block) 
      piece.blocks in 
  stack_blocks stacked_blocks piece_blocks_list;;

let drop_piece world =
  let piece = world.current_piece in 
  let moved_piece = {piece with pos = {piece.pos with y = piece.pos.y + 1}} in 
  if (check_piece_collision moved_piece DOWN) == DOWN then 
    {world with 
      stacked_blocks = stack_piece world.stacked_blocks piece; 
      current_piece = make_a_piece (Random.int 7)}
  else 
    {world with current_piece = moved_piece}

let move_piece_right piece =
  let new_piece = {piece with pos
    = { piece.pos with x = (piece.pos.x + 1)}} in 
  if (check_piece_collision new_piece RIGHT) == NO_COLLISION then 
    new_piece 
  else 
    piece;;

let move_piece_left piece =
  let new_piece = {piece with pos
    = { piece.pos with x = (piece.pos.x - 1)}} in 
  if (check_piece_collision new_piece LEFT) == NO_COLLISION then 
    new_piece 
  else 
    piece;;

let get_time_now () = 
  Unix.gettimeofday ();;

let update_world_with_input world () =
  let event = Graphics.wait_next_event [ Graphics.Poll ] in
  if event.Graphics.keypressed then
    match (read_key ()) with
     'd'      -> set_redraw {world with current_piece = (move_piece_right world.current_piece)}
    |'a'|'q'  -> set_redraw {world with current_piece = (move_piece_left world.current_piece)}
    |'s'      -> set_redraw {world with current_piece = (rotate_piece world.current_piece)}
    | x       -> world
  else
    world;;

let update_world world =
  let new_world = (update_world_with_input world ()) in
  new_world;;

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
  let a = get_pixel_coords({x = left_edge; y = -1}) in 
  let b = get_pixel_coords({x = left_edge; y = bottom_edge}) in 
  let c = get_pixel_coords({x = right_edge + 1; y = bottom_edge}) in 
  let d = get_pixel_coords({x = right_edge + 1; y = -1}) in 
  draw_poly_line [| (a.x_pixel , a.y_pixel) ; 
		    (b.x_pixel , b.y_pixel) ; 
		    (c.x_pixel , c.y_pixel) ; 
		    (d.x_pixel , d.y_pixel) |];;

let draw_world world =
  if world.redraw then 
    (clear_graph ();
     draw_frame ();
     draw_block_list world.stacked_blocks;
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
    stacked_blocks = [];
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
    reset_lap_start (set_redraw (drop_piece world))
  else
    world;;

let rec run world = 
  run (draw_world (finilize_lap (update_world (start_lap world))));;

let main = 
  run (create_world ())
