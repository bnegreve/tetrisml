(*
*  test.ml
*
*  Made by (Benjamin Negrevergne)
*  Login   <benjamin@neb.dyn.cs.kuleuven.be>
*
*  Started on  Sat Oct 12 14:23:30 2013 Benjamin Negrevergne
*  Last update Sat Oct 12 14:23:30 2013 Benjamin Negrevergne
*)


(* To compile this example: ocamlc graphics.cma grtest1.ml -o grtest1 *)
(*#load "graphics.cma";;*)
open Graphics;;
open Printf;;

let block_size = 10;;
let block_padding = 2;;
let screen_width = 640;; 
let screen_height = 480;;
let lap_length = 0.1; (* in sec *)
type block_pos = {x: int; y: int};;
type pixel_pos = {x_pixel: int; y_pixel: int};;

type piece = 
  { pos: block_pos; blocks: block_pos list; color: int};;

type world = {
  current_piece: piece;
  lap_start: float; 
};;

let get_absolute_coords origin point = 
  {x = (origin.x + point.x); y = (origin.y + point.y)};;

let get_pixel_coords pos = 
  { x_pixel = (pos.x * block_size);
    y_pixel = (screen_height - block_size) - (pos.y * block_size); };;
  
let draw_block_with_pixel_coords x y = 
  Printf.printf "Pos: [%d, %d]\n" x y;
  draw_rect x y block_size block_size; 
  fill_rect (x + block_padding) (y + block_padding) 
    (block_size - (2 * block_padding)) (block_size - (2 * block_padding));;

let print_pos pos = 
  Printf.printf "Pos: [%d, %d]\n" pos.x pos.y;;

let draw_block pos =
  print_pos pos;
  let pixel_pos = get_pixel_coords pos in 
  draw_block_with_pixel_coords pixel_pos.x_pixel pixel_pos.y_pixel;;

let make_square_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::{x = 1; y = 0}::{x = 1; y = 1}::{x = 0; y = 1}::[]; 
    color = red; };;

let make_l_shaped_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = -1}::{x = 0; y = 0}::{x = 0; y = 1}::{x = 1; y = 1}::[]; 
    color = blue; };;

let make_single_block_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::[]; 
    color = red; };;

let make_a_piece = 
  make_l_shaped_piece;;

let rec draw_block_list pos blocks =
  List.map (function block -> draw_block (get_absolute_coords pos block)) blocks;;

  
let draw_piece piece = 
  set_color piece.color; 
  draw_block_list piece.pos piece.blocks;;

let push_piece_down piece = 
  {piece with pos
    = { piece.pos with y = (piece.pos.y + 1)}};;

let get_time_now () = 
  Unix.gettimeofday ();;
    

let update world =
  let now = get_time_now () in
  { world with
    lap_start = now;
    current_piece = push_piece_down world.current_piece;
  };;

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

let finilize_lap world =
  wait_until (world.lap_start +. lap_length);
  world;;

let draw_world world =
  clear_graph ();
  draw_piece world.current_piece;
  world;; 

(* MAIN *)

open_graph " 640x480";;
let  world = ref {
  current_piece = make_a_piece;
  lap_start = get_time_now ();
} in while true do
    printf "New lap\n";
    world := finilize_lap (draw_world (update !world))
  done;;

