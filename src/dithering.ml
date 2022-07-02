module Pixel = Image.Pixel
module Color = Image.Color

let rec dither image original =
  let compressed = compress_color original in
  (* TODO move side effect away *)
  Image.set_pixel_at image (Pixel.position original) compressed;
  if has_neighbors_to_dither image original then
    dither_neighbors image original compressed

and compress_color pixel =
  let compressed = Color.compress (Pixel.color pixel) ~factor:1 in
  Pixel.create (Pixel.position pixel) compressed

and has_neighbors_to_dither image p =
  let x, y = Pixel.position p in
  let d = Image.get_dimensions image in
  x != (d.w-1) && y != (d.h-1) && x != 0

and dither_neighbors image original compressed =
  let error = calculate_error original compressed in
  let x, y = Pixel.position original in
  let rules = [
    (x+1, y  ), (7.0 /. 16.0);
    (x-1, y+1), (3.0 /. 16.0);
    (x  , y+1), (5.0 /. 16.0);
    (x+1, y+1), (1.0 /. 16.0);
  ] in
  let f = fun rule -> apply_rule rule image error in
  List.iter f rules

and calculate_error original compressed =
  Color.sub (Pixel.color original) (Pixel.color compressed)

and apply_rule rule img error =
  let (pos, ratio) = rule in
  let pixel = Image.pixel_at img pos in
  let corr = calculate_correction error ratio in
  let new_color = apply_correction pixel corr in
  let new_pixel = Pixel.create pos new_color in
  Image.set_pixel_at img pos new_pixel

and calculate_correction error ratio =
  let mul_ratio = fun x ->
    x
    |> Float.of_int
    |> Float.mul ratio
    |> Int.of_float
  in
  Color.map error ~f:mul_ratio

and apply_correction pixel correction =
  Color.sum (Pixel.color pixel) correction
