module MyPixel = struct
  type t =
    {
        x : int;
        y : int;
        rgb : (int * int * int);
    }
end

module FilePath : sig
  type t

  val of_string : string -> t
end
=
struct
  type t = string

  let of_string s = s
end

module MyImage : sig
  type t

  type dimensions = { w : int; h : int }

  val pixel_at : t -> int -> int -> MyPixel.t

  val set_pixel_at : t -> int -> int -> MyPixel.t -> unit

  val copy : t -> t

  val get_dimensions : t -> dimensions

  (* TODO: this should be in an IO module *)
  (* TODO: maybe use a abstraction for file_path instead of plain string *)
  val get_image : string -> t option

  val set_image : t -> string -> unit

  (* TODO: use MyPixel.t instead of int -> int *)
  val for_each_pixel : t -> (int -> int -> unit) -> unit
end
=
struct
  open Bimage
  open MyPixel

  type t = (int, u8, [`Rgb]) Bimage.Image.t

  type dimensions = { w : int; h : int }

  let pixel_at image x y =
    let r = Image.get image x y 0 in
    let g = Image.get image x y 1 in
    let b = Image.get image x y 2 in
    { x = x; y = y; rgb = (r, g, b) }

  let set_pixel_at image x y pixel =
    let (r, g, b) = pixel.rgb in
    Image.set image x y 0 r;
    Image.set image x y 1 g;
    Image.set image x y 2 b;
    ()

  let copy image = Image.copy image

  let get_dimensions image =
    let (w, h, _) = Image.shape image in
    { w = w; h = h }

  let get_image file_path =
    match Bimage_unix.Magick.read u8 rgb file_path with
    | Ok img -> Some img
    | Error _ -> None

  let set_image image file_path =
    Bimage_unix.Magick.write file_path image

  let for_each_pixel image fn =
    Image.for_each (fun x y _ -> fn x y) image
end

module IO : sig
  val get_image : string -> MyImage.t option

  val set_image : MyImage.t -> string -> unit
end
=
struct
  (* TODO: workaround, check how to fix this *)
  let get_image = MyImage.get_image
  let set_image = MyImage.set_image
end
  
let map_triple (a, b, c) f = (f a, f b, f c)

let sum_triples (a, b, c) (x, y, z) = (a+x, b+y, c+z)

let find_closest_palette_color pixel =
  let open MyPixel in
  let (r, g, b) = pixel.rgb in
  let f x =
    let x_int = Float.of_int x in
    (Float.round (x_int /. 255.0)) *. 255.0
    |> Int.of_float
  in
  { x = pixel.x; y = pixel.y; rgb = (f r, f g, f b) }

let calculate_error old_pixel new_pixel =
  let open MyPixel in
  let (r, g, b) = old_pixel.rgb in
  let (nr, ng, nb) = new_pixel.rgb in
  (r-nr, g-ng, b-nb)

let dither_ngbh pixel error ratio =
  let open MyPixel in
  let error_float = map_triple error Float.of_int in
  let multiplied_by_ratio = map_triple error_float (Float.mul ratio) in
  let mult_int = map_triple multiplied_by_ratio Int.of_float in
  let result = sum_triples pixel.rgb (mult_int) in

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
let floyd_steinberg (pixel : MyPixel.t) x y get set =
  let old_pixel = pixel in
  let new_pixel = find_closest_palette_color old_pixel in
  (* TODO move side effect away *)
  set x y new_pixel;
  let error = calculate_error old_pixel new_pixel in
  dither_neighbors x y error get set

let apply_floyd_steinberg image dimensions x y =
  let open MyImage in
  if x == (dimensions.w-1) || y == (dimensions.h-1) || x == 0
     then ()
  else
    let pixel = pixel_at image x y in
    floyd_steinberg pixel x y (pixel_at image) (set_pixel_at image);
    ()

let main =
  match IO.get_image "input/img.jpg" with
  | None -> failwith ("Error loading image")
  | Some img ->
     let dest = MyImage.copy img in
     let dimensions = MyImage.get_dimensions dest in
     let () =
       MyImage.for_each_pixel dest (fun x y -> apply_floyd_steinberg dest dimensions x y)
     in
     IO.set_image dest "output/floydsteinberg-4.jpg"

let () = main
