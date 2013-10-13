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
let screen_width = 640;;
let screen_height = 480;;
let lap_length = 1.; (* in sec *)
  
type block_pos = {x: int; y: int};;
type pixel_pos = {x_pixel: int; y_pixel: int};;
type piece = 
  { pos: block_pos; blocks: block_pos list; color: int};;
type world = {
  current_piece: piece;
  lap_start: float; 
  redraw: bool; 
};;

let set_redraw world = 
  { world with redraw = true };;

let get_absolute_coords origin point = 
  {x = (origin.x + point.x); y = (origin.y + point.y)};;

let get_pixel_coords pos = 
  { x_pixel = (pos.x * block_size);
    y_pixel = (screen_height - block_size) - (pos.y * block_size); };;
  
let draw_block_with_pixel_coords x y = 
  (* Printf.printf "Pos: [%d, %d]\n" x y; *)
  draw_rect x y block_size block_size; 
  fill_rect (x + block_padding) (y + block_padding) 
    (block_size - (2 * block_padding)) (block_size - (2 * block_padding));;

let print_pos pos = 
  Printf.printf "Pos: [%d, %d]\n" pos.x pos.y;;

let print_world world = 
  Printf.printf "lap_start %f, redraw %B\n" world.lap_start world.redraw; 
world;;

let draw_block pos =
  (* print_pos pos; *)
  let pixel_pos = get_pixel_coords pos in 
  draw_block_with_pixel_coords pixel_pos.x_pixel pixel_pos.y_pixel;;

let make_square_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::{x = 1; y = 0}::{x = 1; y = 1}::{x = 0; y = 1}::[]; 
    color = red; };;

let make_l_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = -1}::{x = 0; y = 0}::{x = 0; y = 1}::{x = 1; y = 1}::[]; 
    color = black; };;

let make_rl_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = -1}::{x = 0; y = 0}::{x = 0; y = 1}::{x = -1; y = 1}::[]; 
    color = blue; };;

let make_t_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::{x = -1; y = 0}::{x = 1; y = 0}::{x = 0; y = 1}::[]; 
    color = green; };;

let make_s_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::{x = 0; y = -1}::{x = 1; y = 0}::{x = 1; y = 1}::[]; 
    color = green; };;

let make_rs_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::{x = 0; y = -1}::{x = -1; y = 0}::{x = -1; y = 1}::[]; 
    color = green; };;

let make_rs_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::{x = 0; y = -2}::{x = 0; y = -1}::{x = 0; y = 1}::[]; 
    color = green; };;

let make_single_block_piece =
  { pos = {x = 0; y = 0};
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
  | 6 -> make_l_shaped_piece
  | x -> make_l_shaped_piece;;

let rotate_piece piece = 
  {piece with blocks = 
      List.map (function block -> { x = block.y; y = -block.x}) piece.blocks};;
    
let rec draw_block_list pos blocks =
  List.map (function block -> draw_block (get_absolute_coords pos block)) blocks;;

  
let draw_piece piece = 
  set_color piece.color; 
  draw_block_list piece.pos piece.blocks;;

let move_piece_down piece = 
  {piece with pos
    = { piece.pos with y = (piece.pos.y + 1)}};;

let move_piece_right piece =
  {piece with pos
    = { piece.pos with x = (piece.pos.x + 1)}};;

let move_piece_left piece =
  {piece with pos
    = { piece.pos with x = (piece.pos.x - 1)}};;

let get_time_now () = 
  Unix.gettimeofday ();;

let update_world_with_input world () =
  let event = Graphics.wait_next_event [ Graphics.Poll ] in
  if event.Graphics.keypressed then
    match read_key () with
      'd' -> set_redraw {world with current_piece = (move_piece_right world.current_piece)}
    |'q' -> set_redraw {world with current_piece = (move_piece_left world.current_piece)}
    |'s' -> set_redraw {world with current_piece = (rotate_piece world.current_piece)}
    | x -> world
  else
    world;;

let drop_piece world = 
  set_redraw { world with current_piece = (move_piece_down world.current_piece) }

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

let draw_world world =
  if world.redraw then 
    (clear_graph ();
     draw_piece world.current_piece;
     {world with redraw = false})
  else 
    world;; 

let create_world () = 
  Random.self_init ();
  open_graph " 640x480";
  {
    current_piece = make_a_piece (Random.int 7);
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
    reset_lap_start (drop_piece world)
  else
    world;;

let rec run world = 
  run (draw_world (finilize_lap (update_world (start_lap world))));;

let main = 
  run (create_world ())
