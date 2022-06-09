open Bimage
open Bimage_unix

type my_pixel =
  {
    x : int;
    y : int;
    rgb : (int * int * int);
  }

let print_pixel (r, g, b) =
  print_string ((string_of_int r) ^ "," ^  (string_of_int g) ^ "," ^ (string_of_int b))

let print_pixel_float (r, g, b) =
  print_string ((string_of_float r) ^ "," ^  (string_of_float g) ^ "," ^ (string_of_float b))

let map_triple (a, b, c) f = (f a, f b, f c)

let sum_triples (a, b, c) (x, y, z) = (a+x, b+y, c+z)

let find_closest_palette_color pixel =
  let (r, g, b) = pixel.rgb in
  let f x =
    let x_int = Float.of_int x in
    (Float.round (x_int /. 255.0)) *. 255.0
    |> Int.of_float
  in
  { x = pixel.x; y = pixel.y; rgb = (f r, f g, f b) }

let calculate_error old_pixel new_pixel =
  let (r, g, b) = old_pixel.rgb in
  let (nr, ng, nb) = new_pixel.rgb in
  (r-nr, g-ng, b-nb)

let dither_ngbh pixel error ratio =
  let error_float = map_triple error Float.of_int in
  let multiplied_by_ratio = map_triple error_float (Float.mul ratio) in
  let mult_int = map_triple multiplied_by_ratio Int.of_float in
  let result = sum_triples pixel.rgb (mult_int) in

  (* if (pixel.x<3 && pixel.y<3) then ( *)
  (*   print_string "error_float: "; print_pixel_float error_float; print_string "\n"; *)
  (*   print_string "multiplied_by_ratio: "; print_pixel_float multiplied_by_ratio; print_string "\n"; *)
  (*   print_string "result: "; print_pixel result; print_string "\n"; *)
  (*   print_string "\n"; *)
  (*  ); *)

  { x = pixel.x ; y = pixel.y ; rgb = result }

let do_it x y ratio error get set =
  let pixel = (get x y) in
  set x y (dither_ngbh pixel error ratio);
  ()

let dither_neighbors x y error get set =
  do_it (x+1) y (7.0 /. 16.0) error get set;
  do_it (x-1) (y+1) (3.0 /. 16.0) error get set;
  do_it x (y+1) (5.0 /. 16.0) error get set;
  do_it (x+1) (y+1) (1.0 /. 16.0) error get set;
  ()

let debug_dither_neighbors x y error get set =
  if x < 5 || y < 5 then
    (print_string ((string_of_int x) ^ "," ^  (string_of_int y) ^ "\n");
    dither_neighbors x y error get set)
  else
    ()

(* TODO: handle first column and row *)
let floyd_steinberg pixel x y get set =
  let old_pixel = pixel in
  let new_pixel = find_closest_palette_color old_pixel in
  (* TODO move side effect away *)
  set x y new_pixel;
  let error = calculate_error old_pixel new_pixel in
  (* if x<3 && y<3 *)
  (* then ( *)
  (*    print_string "old_pixel: "; print_pixel old_pixel.rgb; print_string "\n"; *)
  (*    print_string "new_pixel: "; print_pixel new_pixel.rgb; print_string "\n"; *)
  (*    print_string "error: "; print_pixel error; print_string "\n"; *)
  (*    print_string "\n"; *)
  (* ); *)
  dither_neighbors x y error get set

let get_adapt dest x y =
    let r = Image.get dest x y 0 in
    let g = Image.get dest x y 1 in
    let b = Image.get dest x y 2 in
    { x = x; y = y; rgb = (r, g, b) }

let set_adapt dest x y pixel_to_save =
    let (r, g, b) = pixel_to_save.rgb in
    Image.set dest x y 0 r;
    Image.set dest x y 1 g;
    Image.set dest x y 2 b;
    ()

let main =
  let input_image =
    match Magick.read u8 rgb "input/img.jpg" with
    | Ok img -> img
    | Error e -> failwith (Error.to_string e)
  in

  let dest = Image.copy input_image in
  let (w, h, _) = Image.shape dest in

  (* then print_string ((string_of_int x) ^ "," ^  (string_of_int y) ^ "\n") *)
  let _ = Image.for_each (fun x y _px ->
              if x == (w-1) || y == (h-1) || x == 0
              then ()
              else
                let pixel = get_adapt dest x y in
                floyd_steinberg pixel x y (fun x y -> get_adapt dest x y) (fun x y pixel_to_save ->
                    set_adapt dest x y pixel_to_save;
                    )
                )
    dest
  in

  Magick.write "output/floydsteinberg.jpg" dest

let () = main
