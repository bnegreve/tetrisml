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


let block_size = 10;;
let block_padding = 2;;
let screen_width = 640;; 
let screen_height = 480;;

type block_pos = {x: int; y: int};;
type pixel_pos = {x_pixel: int; y_pixel: int};;

let get_absolute_coords origin point = 
  {x = (origin.x + point.x); y = (origin.y + point.y)};;

let get_pixel_coords pos = 
  { x_pixel = (pos.x * block_size);
    y_pixel = (screen_height - block_size) - (pos.y * block_size); };;
  
let draw_block_with_pixel_coords x y = 
  Printf.fprintf stdout "Pos: [%d, %d]\n" x y;
  draw_rect x y block_size block_size; 
  fill_rect (x + block_padding) (y + block_padding) 
    (block_size - (2 * block_padding)) (block_size - (2 * block_padding));;

let print_pos pos = 
  Printf.fprintf stdout "Pos: [%d, %d]\n" pos.x pos.y;;

let draw_block pos =
  print_pos pos;
  let pixel_pos = get_pixel_coords pos in 
  draw_block_with_pixel_coords pixel_pos.x_pixel pixel_pos.y_pixel;;

type piece = 
  { pos: block_pos; blocks: block_pos list; color: int};;

let make_square_shaped_piece =
  { pos = {x = 1; y = 1};
    blocks = {x = 0; y = 0}::{x = 1; y = 0}::{x = 1; y = 1}::{x = 0; y = 1}::[]; 
    color = red; };;

let make_single_block_piece =
  { pos = {x = 0; y = 0};
    blocks = {x = 0; y = 0}::[]; 
    color = red; };;

let rec draw_block_list pos blocks =
  List.map (function block -> draw_block (get_absolute_coords pos block)) blocks;;

  
let draw_piece piece = 
  set_color piece.color; 
  draw_block_list piece.pos piece.blocks;;

open_graph " 640x480";;
(* draw_piece make_single_block_piece;; *)
draw_piece make_square_shaped_piece;;
read_line ();;
