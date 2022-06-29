open Image.Pixel

(* TODO: refactor *)
let map_triple (a, b, c) f = (f a, f b, f c)

let sum_triples (a, b, c) (x, y, z) = (a+x, b+y, c+z)

let rec dither image pixel =
  let old_pixel = pixel in
  let new_pixel = find_closest_palette_color old_pixel in
  (* TODO move side effect away *)
  Image.set_pixel_at image pixel.x pixel.y new_pixel;
  let error = calculate_error old_pixel new_pixel in
  dither_neighbors image pixel error

and find_closest_palette_color pixel =
  let (r, g, b) = pixel.rgb in
  let f x =
    let factor = 1.0 in
      let x_int = Float.of_int x in
      (Float.round (factor *. x_int /. 255.0)) *. (255.0 /. factor)
      |> Int.of_float
  in
  { x = pixel.x; y = pixel.y; rgb = (f r, f g, f b) }


and calculate_error old_pixel new_pixel =
  let (r, g, b) = old_pixel.rgb in
  let (nr, ng, nb) = new_pixel.rgb in
  (r-nr, g-ng, b-nb)

and dither_neighbors image pixel error =
  if should_apply_dithering_to_neighbors image pixel
     then
      let x = pixel.x in
      let y = pixel.y in
      push_residual_quantization_error image (x+1) (y  ) (7.0 /. 16.0) error;
      push_residual_quantization_error image (x-1) (y+1) (3.0 /. 16.0) error;
      push_residual_quantization_error image (x  ) (y+1) (5.0 /. 16.0) error;
      push_residual_quantization_error image (x+1) (y+1) (1.0 /. 16.0) error
  else 
    ()

and should_apply_dithering_to_neighbors image p =
  let d = Image.get_dimensions image in
  p.x != (d.w-1) && p.y != (d.h-1) && p.x != 0

and push_residual_quantization_error image x y ratio error =
  let pixel = (Image.pixel_at image x y) in
  let new_pixel = calculate_neighbor_correction pixel error ratio in
  Image.set_pixel_at image x y new_pixel

and calculate_neighbor_correction pixel error ratio =
  let error_float = map_triple error Float.of_int in
  let multiplied_by_ratio = map_triple error_float (Float.mul ratio) in
  let mult_int = map_triple multiplied_by_ratio Int.of_float in
  let result = sum_triples pixel.rgb (mult_int) in
  { x = pixel.x ; y = pixel.y ; rgb = result }
